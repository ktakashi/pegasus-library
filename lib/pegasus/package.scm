;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; pegasus/package.scm - Package
;;;  
;;;   Copyright (c) 2010-2013  Takashi Kato  <ktakashi@ymail.com>
;;;   
;;;   Redistribution and use in source and binary forms, with or without
;;;   modification, are permitted provided that the following conditions
;;;   are met:
;;;   
;;;   1. Redistributions of source code must retain the above copyright
;;;      notice, this list of conditions and the following disclaimer.
;;;  
;;;   2. Redistributions in binary form must reproduce the above copyright
;;;      notice, this list of conditions and the following disclaimer in the
;;;      documentation and/or other materials provided with the distribution.
;;;  
;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;  

#!read-macro=sagittarius/regex
(library (pegasus package)
    (export retrieve-package
	    install-package
	    clean-package
	    append-child-dependency
	    check-version
	    installed-package-info
	    remove-package
	    run-tests
	    ;; might want to extend for future as plugin
	    check-result)
    (import (rnrs)
	    (rnrs eval)
	    (rnrs mutable-pairs)
	    (clos user)
	    (archive)
	    (rfc http)
	    (rfc uri)
	    (rfc gzip)
	    (util file)
	    (util port)
	    (sagittarius)
	    (sagittarius io)
	    (sagittarius control)
	    (sagittarius object)
	    (sagittarius process)
	    (sagittarius regex)
	    (pegasus repository)
	    (pegasus config)
	    (pegasus formula)
	    ;; must be latter than (sagittarius)
	    (scheme load) ;; for testing need R7RS load
	    (srfi :1 lists)
	    (srfi :13 strings)
	    (srfi :26 cut)
	    (srfi :39 parameters)
	    (srfi :64 testing))

  (define (retrieve-package formula :key (verbose #f))
    (define saved-dir (current-directory))
    ;; might be better to use temporary file
    ;; so that it won't use much memory
    (define (retrieve-source url)
      (let-values (((scheme user-info host port path query frag)
		    (uri-parse url)))
	(when verbose (format #t "-- Retrieving: ~a~%" url))
	(cond ((and scheme 
		    (or (string=? "http" scheme) (string=? "https" scheme)))
	       (let*-values (((server path) (url-server&path url))
			     ((status _ body) 
			      (http-get server path
					:secure (string=? "https" scheme)
					:receiver (http-binary-receiver))))
		 (unless (string=? status "200")
		   (error 'retrieve-package "failed to retrieve source archive"
			  status server path))
		 body))
	      ((or (not scheme) (string=? "file" scheme)) ;; mostly for testing
	       ;; don't consider windows that much ...
	       (if (file-exists? path)
		   (file->bytevector path)
		   (let ((p (build-path saved-dir path)))
		     (file->bytevector p))))
	      (else
	       (error 'retrieve-package "scheme is not supported" scheme)))))
    ;; FIXME ugly
    (define (find-working-directory mark contents work-dir)
      (if (file-exists? mark)
	  work-dir
	  ;; find directory
	  (let ((d (find (lambda (p)
			   (and (not (string=? "." p))
				(not (string=? ".." p))
				(file-directory? p))) contents)))
	    ;; directory must be only one!!
	    (unless d (error 'retrieve-package 
			     "could not find working directory"))
	    (parameterize ((current-directory d))
	      (find-working-directory mark (read-directory (current-directory))
				      (build-path work-dir d))))))


    (let ((work (work-directory)))
      (parameterize ((current-directory work))
	(let-values (((package-type source) (formula-package formula)))
	  (let ((type (~ source 'type))
		(url  (~ source 'url))
		(dest (~ formula 'name)))
	    (if (eq? package-type 'repository)
		;; get repository
		(let ((version (~ source 'version))
		      (rep (repository type url)))
		  (clone-repository rep dest)
		  (when version 
		    (parameterize ((current-directory dest))
		      (update-repository rep version)))
		  dest)
		;; download or just open... 
		(let ((body (retrieve-source url)))
		  (define (get-source-input body compression)
		    ;; TODO generic compression
		    (unless (or (not compression) (eq? compression 'gzip))
		      (error 'retrieve-package "not supported" compression))
		    (let ((in (open-bytevector-input-port body)))
		      (if (eq? compression 'gzip)
			  (open-gzip-input-port in)
			  in)))
		  (define (destinator e)
		    (let ((name (archive-entry-name e)))
		      ;; TODO extract only required file
		      (when verbose (format #t "-- Extracting: ~a~%" name))
		      name))
		  (let* ((compression (and (slot-bound? source 'compression)
					   (~ source 'compression)))
			 (in (get-source-input body compression))
			 (version (~ formula 'version))
			 (work-dir (format "~a-~a" dest version)))
		    (create-directory* work-dir)
		    (parameterize ((current-directory work-dir))
		      (call-with-archive-input type in
		        (cut extract-all-entries <> :destinator destinator
			     :overwrite #t))
		      ;; TODO find target directory!
		      (let* ((install (~ formula 'install))
			     (files   (~ install 'files))
			     (directories (~ install 'directories))
			     (contents (read-directory (current-directory))))
			(if (not (null? files)) ;; this is easier
			    (find-working-directory (car files)
						    contents work-dir)
			    (find-working-directory 
			     (car (map car directories)) contents work-dir))))
		    ))))))))

  (define system-install-dir (sagittarius-sitelib-directory))

  ;; should be in the (util file) but hmmmm, one of those inconsidered APIs
  (define (copy-directory* base-path dst
			   :key (excludes '()))
    (filter values
	    (path-map
	     base-path
	     (lambda (opath type)
	       (and-let* (( (not (member opath excludes string-contains)) )
			  (path (string-copy opath 
					     (+ (string-length base-path) 1)))
			  (dest (build-path dst path)))
		 (if (eq? type 'directory)
		     (begin (create-directory* dest) dest)
		     (let-values (((dir base ext) (decompose-path path)))
		       (when dir (create-directory* (build-path dst dir)))
		       (copy-file opath dest #t)
		       dest)))))))

  (define (install-package formula work :key (verbose #f))
    (let* ((path (build-path (work-directory) work))
	   ;; use absolute path for convenience
	   (installed (absolute-path (installed-directory))))
      (parameterize ((current-directory path))
	;; copy file and directory
	(let* ((install (~ formula 'install))
	       (files   (~ install 'files))
	       (directories (~ install 'directories)))
	  (define (copy-files files)
	    (map (lambda (f)
		   (let ((dest (build-path system-install-dir f)))
		     (when verbose
		       (format #t "-- Installing: ~a~%" dest))
		     (let-values (((dir base ext) (decompose-path dest)))
		       (unless (file-exists? dir)
			 (create-directory* dir)))
		     (copy-file f dest #t)
		     dest)) files))
	  (define (copy-dirs directories)
	    (append-map 
	     (lambda (d)
	       (when verbose
		 (format #t "-- Installing: ~a~%" 
			 (build-path system-install-dir (car d))))
	       (let* ((options (cdr d))
		      (files (apply copy-directory* (car d) 
				    system-install-dir options)))
		 (filter-map (lambda (f/d)
			       (if (file-directory? f/d) #f f/d)) files)))
	     directories))
	  ;; TODO install prefix
	  
	  (let ((installed-files (copy-files files))
		(installed-dirs  (copy-dirs directories))
		(installed-info-file (build-path installed (~ formula 'name))))
	    (when (file-exists? installed-info-file)
	      (delete-file installed-info-file))
	    (call-with-output-file installed-info-file
	      (lambda (out)
		;; ((version $varsion) (children) (files ...))
		;; children will be added when a library depending on this
		;; library is installed
		(write `((version ,(~ formula 'version))
			 (children)
			 (files ,@installed-files ,@installed-dirs))
		       out))))))))

  (define (clean-package work :key (verbose #f))
    ;; delete work directory
    (let ((path (work-directory)))
      (parameterize ((current-directory path))
	(when verbose (format #t "-- Deleting working directory: ~a~%" work))
	(delete-directory* work))))

  (define (check-version formula callback)
    (let-values (((package-version child-dependencies files)
		  (installed-package-info (~ formula 'name))))
      (unless (or (not package-version)
		  (string=? package-version (~ formula 'version)))
	(format #t 
		"WARNING: '~a' is already installed: installed=~a, tareget=~a~%"
		(~ formula 'name) package-version (~ formula 'version))
	(if callback
	    (begin
	      (display "Install it anyway?")
	      (callback))
	    #t))))

  (define (append-child-dependency parent-name child-formula)
    (let ((path (build-path (installed-directory) parent-name)))
      (call-with-port (open-file-input/output-port 
		       path (file-options no-fail no-truncate)
		       (buffer-mode block) (native-transcoder))
	(lambda (in/out)
	  (let ((pos (port-position in/out))
		(info (read in/out)))
	    (cond ((assq 'children info) =>
		   (lambda (slot)
		     ;; should we put version as well?
		     (let ((name (~ child-formula 'name)))
		       (unless (assoc name (cdr slot))
			 (set-cdr! slot (acons name (~ child-formula 'version)
					       (cdr slot)))))))
		  ;; something is wrong but ignore
		  (else #f))
	    (set-port-position! in/out pos)
	    (write info in/out))))))

  (define (installed-package-info package)
    (let ((file (build-path (installed-directory) package)))
      (if (file-exists? file)
	  (let ((contents (car (file->sexp-list file))))
	    (values (cadr (assq 'version contents))
		    (cdr  (assq 'children contents))
		    (cdr  (assq 'files contents))))
	  (values #f '() '()))))

  (define (remove-package formula :key (verbose #f))
    (let-values (((package-version child-dependencies files)
		  (installed-package-info (~ formula 'name))))
      ;; TODO cascade?
      (if (null? child-dependencies)
	  (let ((dir (installed-directory))
		(dependencies (~ formula 'dependencies)))
	    (for-each (lambda (file)
			(when verbose (format #t "Uninstalling: ~a~%" file))
			(when (file-exists? file)
			  (delete-file file))) files)
	    ;; remove child-dependency info from installed
	    (when dependencies
	      (for-each (lambda (d)
			  (let ((file (build-path dir (~ d 'name))))
			    ;; TODO version?
			    (let ((info (car (file->sexp-list file))))
			      (cond ((assq 'children info) =>
				     (lambda (slot)
				       (let ((name (~ formula 'name)))
					 ;; remove it
					 (set-cdr! slot
					  (remp (lambda (s)
						  (string=? (car s) name))
						(cdr slot))))))
				    ;; something is wrong but ignore
				    (else #f))
			      (delete-file file)
			      (call-with-output-file file
				(lambda (out) (write/ss info out))))))
			dependencies))
	    ;; remove info file
	    (let ((f (build-path dir (~ formula 'name))))
	      (when (file-exists? f) (delete-file f))
	      #t))
	  (format #t "Child dependencies found: ~a~%" child-dependencies))))

  (define (open-test-runner-port stdout sink)
    (define (write! str start count)
      (put-string stdout str start count)
      (put-string sink   str start count)
      count)
    (make-custom-textual-output-port "test-runner-port" write! #f #f #f))

  ;; check if the result is ok
  (define-method check-result (style result env)
    (error 'check-result "style not supported" style))

  ;; TODO check the result
  (define-method check-result ((style (eql 'srfi-64)) result env)
    ;; means we might have custom error reporter thus we need to
    ;; check if all tests are passed.
    ;; simple check
    (and (not (#/unexpected/ result))
	 (let ((runner (eval '(test-runner-current) env)))
	   (and (zero? (test-runner-xpass-count runner))
		(zero? (test-runner-fail-count runner))))))

  (define-method check-result ((style (eql 'srfi-78)) result env)
    ;; read line and check default srfi 78 report style...
    ;; TODO add more checks pattern
    (let* ((in (open-string-input-port result))
	   (checks (port-map (lambda (s)
			       (cond ((#/(\d+)\s+failed\./ s) =>
				      (lambda (m) (string=? "0" (m 1))))
				     (else #t)))
			     (cut get-line in))))
      (for-all (cut eqv? #t <>) checks)))

  (define (run-test test verbose)
    (let-values (((sink extractor) (open-string-output-port)))
      (define stdout (current-output-port))
      (define env (environment '(only (sagittarius) 
				      import library define-library)))
      (parameterize ((load-path (load-path)) ;; preserve load path
		     (current-output-port (open-test-runner-port
					   (current-output-port) sink)))
	;; now we can add :)
	(add-load-path (~ test 'loadpath))
	(when verbose (format stdout "-- Running test: ~a~%" (~ test 'file)))
	(load (~ test 'file) env))
      (let1 result (extractor)
	(not (check-result (~ test 'style) result env)))))

  (define (run-tests formula work-dir :key (verbose #f))
    (parameterize ((current-directory (build-path (work-directory) work-dir)))
      (let1 tests (~ formula 'tests)
	(if tests
	    (filter-map (cut run-test <> verbose) tests)
	    '()))))
	  
	
      
  )

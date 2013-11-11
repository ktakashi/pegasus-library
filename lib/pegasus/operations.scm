;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; pegasus/operations.scm - Operational commands
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

(library (pegasus operations)
    (export install
	    remove
	    define-command
	    lookup-command
	    all-commands
	    *prompt*)
    (import (rnrs) 
	    (pegasus config)
	    (pegasus formula)
	    (pegasus package)
	    (clos user)
	    (sagittarius)
	    (sagittarius control)
	    (sagittarius object)
	    (getopt)
	    (util file)
	    (srfi :1 lists)
	    (srfi :26 cut)
	    (srfi :39 parameters))


  (define +command-table+ (make-eq-hashtable))
  (define +help-table+ (make-eq-hashtable))
  (define-syntax define-command
    (lambda (x)
      (syntax-case x ()
	((k (name args ... . rest) help-string body ...)
	 (string? (syntax->datum #'help-string))
	 #'(begin
	     (define (name args ... . rest) body ...)
	     (hashtable-set! +command-table+ 'name name)
	     (hashtable-set! +help-table+ 'name help-string))))))

  (define (all-commands . ignore) 
    (let-values (((keys entries) (hashtable-entries +help-table+)))
      (vector-sort (lambda (a b)
		     (string<=? (symbol->string (car a))
				(symbol->string (car b))))
		   (vector-map cons keys entries))))
  (define (lookup-command name :optional (fallback all-commands))
    (hashtable-ref +command-table+ name fallback))

  ;; TODO implement user input prompt
  (define *prompt* (make-parameter (lambda ()#f)))
  
  ;; TODO add options
  (define-command (install package . rest)
    "install package [-v]\n\
     Installs the specified package.\n\
     Options:\n  \
       -v, --verbose\tShows installing process\n"
    (with-args rest
	((verbose (#\v "verbose") #f #f)
	 . ignore)
      (or
       (and-let* ((formula (find-formula package)))
	 (let* ((dependencies (find-dependencies formula))
		;; resolve dependencies
		(results (filter-map 
			  (lambda (package&version) 
			    (let ((p (car package&version))
				  (v (cdr package&version)))
			      (let-values (((version dep files) 
					    (installed-package-info p)))
				;; TODO check version?
				(and (not (and version (string=? version v)))
				     ;; return #f if succeed to make
				     ;; results empty
				     (not (null? 
					   (apply (lookup-command 'install)
						  `(,p ,@(if verbose
							     '("-v")
							     '())))))
				     ;; for information return package name
				     p))))
			  dependencies)))
	   ;; check version
	   (cond ((not (null? results)) results)		 
		 ((check-version formula (*prompt*))
		  ;; clone repository or get and unpack archiver
		  (guard (e (else 
			     (display "*ERROR* " (current-error-port))
			     (when (who-condition? e)
			       (format (current-error-port) "~a " 
				       (condition-who e)))
			     (when (message-condition? e)
			       (format (current-error-port) "~a"
				       (condition-message e)))
			     (newline (current-error-port))
			     (list package)))
		    (let1 work-dir (retrieve-package formula :verbose verbose)
		      (when (null? (run-tests formula work-dir :verbose verbose))
			(install-package formula work-dir :verbose verbose)
			;; add child dependencies to parents
			(for-each (cut append-child-dependency <> formula) 
				  (map car dependencies)))
		      (clean-package work-dir :verbose verbose)
		      '())))
		 (else (list package)))))
       (begin
	 (format (current-error-port) 
		 "*ERROR* formula not found for ~a~%" package)
	 '()))))
  
  (define-command (remove package . rest)
    "remove package [-c|v]\n\
     Removes the specified package\n\
     Options:\n  \
       -c,--cascade\tRemove child dependencies\n  \
       -v,--verbose\tShow removing process\n"
    (with-args rest
	((cascade (#\c "cascade") #f #f) 
	 (verbose (#\v "verbose") #f #f)
	 . ignore)
      (let1 formula (if (is-a? package <formula>)
			package
			(find-formula package))
	(remove-package formula :verbose verbose))))

  (define-command (init . rest)
    "init [-n=master] [-r=https://github.com/ktakashi/pegasus.git]\n\
     Initialise repository\n\
     Options:\n  \
       -n,--name\tNick name of this repository\n  \
       -r,--repository\tURL of the repository (must be Git repository)\n"
    (define (err . msgs)
      (for-each (lambda (msg) (display msg (current-error-port))) msgs)
      (newline (current-error-port)))
    (with-args rest
	((name (#\n "name") #t "master")
	 (repo (#\r "repository") #t "https://github.com/ktakashi/pegasus.git")
	 . ignore)
      (create-config-file name repo)
      ;;
      (let ((config (read-configuration)))
	(guard (e (else 
		   (err "***ERROR***")
		   (err "Failed to initialise the repository.")
		   (err "Run following command to update manually; ")
		   (err "$ cd " (*configuration-directory*))
		   (for-each (lambda (rep)
			       (err "$ git clone " (cadr rep) " " (car rep)))
			     (~ config 'repositories))))
	  (parameterize ((current-directory (*configuration-directory*)))
	    (for-each (lambda (rep)
			(clone-repository (make-repository-context 'git
								   (cadr rep))
					  (car rep)))
		      (~ config 'repositories)))))
      ))

  (define-command (update . rest)
    "update\n\
     Updates the repository."
    (define (err . msgs)
      (for-each (lambda (msg) (display msg (current-error-port))) msgs)
      (newline (current-error-port)))
    (let ((config (read-configuration)))
      (guard (e (else 
		 (err "***ERROR***")
		 (err "Failed to update the repository.")
		 (err "Run following command to update manually; ")
		 (err "$ cd " (*configuration-directory*))
		 (for-each (lambda (rep) (err "$ git pull " (cadr rep)))
			   (~ config 'repositories))))
	(parameterize ((current-directory (*configuration-directory*)))
	  (for-each (lambda (rep)
		      (sync-repository (make-repository-context 'git
								(cadr rep))))
		    (~ config 'repositories))))))

  (define-command (search . rest)
    "search [-p=$pattern]\n\
     Search repository."
    (with-args rest
	((pattern (#\p "pattern") #t #f)
	 . ignore)
      (let ((files (find-formulas pattern)))
	(for-each (lambda (file)
		    ;; read and get description
		    (let ((formula (call-with-input-file file read))
			  (name (path-sans-extension (path-basename file))))
		      (format #t "~20a: ~a~%" name 
			      (cond ((assq 'description formula) => cadr)
				    (else "No description")))))
		  files))))

  (define-command (help . rest)
    "help command [command ...]\n\
     Show help for target commands\n"
    (for-each (lambda (command)
		(let* ((name (string->symbol command))
		       (doc  (hashtable-ref +help-table+ name #f)))
		  (when doc
		    (format #t "~a~%" doc))))
	      rest))
  
)
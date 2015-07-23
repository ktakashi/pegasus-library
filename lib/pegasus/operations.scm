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
    (import (except (rnrs) remove)
	    (pegasus config)
	    (pegasus formula)
	    (pegasus package)
	    (pegasus repository)
	    (clos user)
	    (sagittarius)
	    (sagittarius control)
	    (sagittarius object)
	    (getopt)
	    (rfc uri)
	    (rfc http)
	    (util file)
	    (except (srfi :1 lists) remove)
	    (srfi :13 strings)
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
  
  (define (default-prompt)
    (let retry ()
      (display "[y/n]> ")
      (flush-output-port (current-output-port))
      (let loop ()
	(let1 l (get-line (current-input-port))
	  (cond ((string-ci=? l "y") #t)
		((string-ci=? l "n") #f)
		(else 
		 (display "'y' or 'n' is required. ")
		 (retry)))))))
  (define *prompt* (make-parameter default-prompt))

  (define (user-prompt prefix)
    (define prompt (*prompt*))
    (lambda ()
      (when prompt
	(format #t "~a  Install it anyway?" prefix)
	(flush-output-port (current-output-port))
	(prompt))))

  (define (find/retrieve-formula package url file)
    (cond (url
	   (let-values (((scheme specific) (uri-scheme&specific url)))
	     ;; for now only supports http(s)
	     (unless (string? scheme) (error 'install "Invalid URL" url))
	     (unless (string-prefix? "http" scheme)
	       (error 'install "URL scheme is not supported" url))
	     (let*-values (((server path) (url-server&path url))
			   ((status header body) 
			    (http-get server path
				      :secure (string=? scheme "https")
				      :receiver (http-string-receiver))))
	       (if (string=? status "200")
		   (read-formula package (open-string-input-port body))
		   (begin
		     (format (current-error-port)
			     "*ERROR* failed to reado formula for ~a from ~a~%"
			     package url)
		     #f)))))
	  (file 
	   (if (file-exists? file)
	       (call-with-input-file file
		 (lambda (in) (read-formula package in)))
	       #f))
	  (else (find-formula package))))

  ;; TODO add options
  (define-command (install package . rest)
    "install pacakge [-u url][-v][-i file]\n\n\
     Installs the specified package.\n\
     Options:\n  \
       -u, --url\n     \
        Retrieve formula from speficied URL.\n     \
        This doesn't resolve dependency by URL.\n  \
       -i, --file\n     \
        Specifies formula with given file.\n     \
        This is testing purpose.\n  \
        If -u is specified with -f, then -u is used.\n  \
       -v, --verbose\tShows installing process\n"
    (with-args rest
	((url     (#\u "url") #t #f)
	 (file    (#\i "file") #t #f)
	 (verbose (#\v "verbose") #f #f)
	 . ignore)
      (or
       (and-let* ((formula (find/retrieve-formula package url file)))
	 (let* ((dependencies (~ formula 'dependencies))
		;; resolve dependencies
		(results (filter-map 
			  (lambda (dependency) 
			    (let ((p (~ dependency 'name))
				  (v (~ dependency 'version)))
			      (let-values (((version dep files) 
					    (installed-package-info p)))
				;; return #f if succeed to make
				;; results empty
				(and (not (null? 
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
			     (when (irritants-condition? e)
			       (format (current-error-port) " ~a"
				       (condition-irritants e)))
			     (newline (current-error-port))
			     (list package)))
		    (define work-dir (retrieve-package formula
						       :verbose verbose))
		    (unless (execute-script formula 'pre work-dir (*prompt*)
					    :verbose verbose)
		      (error 'pre-script-execution "Failed to execute script"))
		    (let ((test-results (run-tests formula work-dir
						   :verbose verbose)))
		      (when (or (null? test-results)
				((user-prompt "Test failed:")))
			(install-package formula work-dir :verbose verbose)
			;; add child dependencies to parents
			(for-each (cut append-child-dependency <> formula) 
				  (map (lambda (d) (~ d 'name)) dependencies))
			(execute-script formula 'post work-dir #f  
					:verbose verbose))
		      (clean-package work-dir :verbose verbose)
		      '())))
		 (else (list package)))))
       (begin
	 (format (current-error-port) 
		 "*ERROR* formula not found for ~a~%" package)
	 '()))))
  
  (define-command (remove package . rest)
    "remove package [-c|v][-u url][-i file]\n\n\
     Removes the specified package\n\
     Options:\n  \
       -c,--cascade\tRemoves child dependencies\n  \
       -u,--url\tUses formula on specified URL\n  \
       -i,--file\tUses formula on specified file\n  \
       -v,--verbose\tShows removing process\n"

    (with-args rest
	((cascade (#\c "cascade") #f #f) 
	 (verbose (#\v "verbose") #f #f)
	 (url     (#\u "url") #t #f)
	 (file    (#\i "file") #t #f)
	 . ignore)
      (let1 formula (if (is-a? package <formula>)
			package
			(find/retrieve-formula package url file))
	(remove-package formula :verbose verbose :cascade cascade))))

  (define-command (init . rest)
    "init [-n=master] [-r=https://github.com/ktakashi/pegasus.git]\n\n\
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
		   (report-error e)
		   (err "***ERROR***")
		   (err "Failed to initialise the repository.")
		   (err "Run following command to update manually; ")
		   (err "$ cd " (*configuration-directory*))
		   (for-each (lambda (rep)
			       (err "$ git clone " (cadr rep) " " (car rep)))
			     (~ config 'repositories))))
	  (parameterize ((current-directory (*configuration-directory*)))
	    (for-each (lambda (rep)
			(clone-repository (repository 'git (cadr rep))
					  (car rep)))
		      (~ config 'repositories)))))
      ))

  (define-command (update . rest)
    "update\n\n\
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
	(for-each 
	 (lambda (rep)
	   (parameterize ((current-directory 
			   (build-path (*configuration-directory*)
				       (car rep))))
	     (sync-repository (repository 'git (cadr rep)))))
	 (~ config 'repositories)))))

  (define-command (search . rest)
    "search [-p $pattern]\n\n\
     Searches repository.\n\
     If no pattern is specified, then it shows all packages.\n\n\
     Options:\n  \
       -p $pattern, --pattern $pattern  specifies pattern of package."
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

  (define-command (desc package . rest)
    "desc package\n\n\
     Shows detail information of the given package."
    (and-let* ((formula (find-formula package)))
      (format #t "Name: ~a~%" (~ formula 'name))
      (format #t "Description: ~a~%" (or (~ formula 'description)
					 "No description"))
      (format #t "Version: ~a~%" (~ formula 'version))
      (for-each (lambda (d)
		  (format #t "Dependent on: ~a [~a]~%" 
			  (~ d 'name) (~ d 'version)))
		(~ formula 'dependencies))
      (and-let* ((a (~ formula 'author)))
	(format #t "Author: ~a<~a>~%"  (~ a 'name) (~ a 'email)))
      (and-let* ((w (~ formula 'homepage)))
	(format #t "Project site: ~a~%" (~ w 'url)))
      (and-let* ((v (~ formula 'sagittarius-version)))
	(format #t "Required Sagittarius version: ~a <= ~a~%"
		(or (~ v 'minimum) "no limit")
		(or (~ v 'maximum) "no limit")))))

  (define-command (help . rest)
    "help command [command ...]\n\n\
     Shows help for target commands\n"
    (for-each (lambda (command)
		(let* ((name (string->symbol command))
		       (doc  (hashtable-ref +help-table+ name #f)))
		  (when doc (format #t "pegasus ~a~%" doc))))
	      rest))
  
)

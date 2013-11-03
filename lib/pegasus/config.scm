;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; pegasus/config.scm - Configuration
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

(library (pegasus config)
    (export *configuration-directory*
	    *configuration-filename*
	    +formula+ +installed+ +work+
	    create-config-file
	    read-configuration
	    ;; meta directories
	    work-directory
	    installed-directory
	    )
    (import (rnrs)
	    (sagittarius)
	    (srfi :26 cut)
	    (srfi :39 parameters)
	    (util file)
	    (clos user))

  ;; for testing we need to make this parameter
  (define *configuration-directory*
    (make-parameter
     (cond ((getenv "PEGASUS_CONFIG_DIR"))  ;; this is the strongest
	   (else
	    (let ((home (or (getenv "HOME") ;; for Cygin or POSIX
			    (getenv "USERPROFILE") ;; for windows
			 )))
	      (build-path home ".pegasus"))))))

  (define *configuration-filename* (make-parameter "init.scm"))

  (define-constant +formula+   "formula")
  (define-constant +installed+ "installed")
  (define-constant +work+      "work")

  (define-class <configuration> ()
    ((repositories :init-keyword :repositories :init-value '())))

  (define (make-configuration sexp)
    (when (or (null? sexp) (not (eq? (car sexp) 'pegasus)))
      (error 'make-configuration "not a configuration" sexp))
    (let ((conf (cdr sexp)))
      (make <configuration>
	:repositories (cond ((assq 'repositories conf) => cdr)
			    (else '())))))

  ;; the very first init.scm
  ;; TODO multi repository, append config or so.
  (define (create-config-file nickname repository)
    (let ((init-file (build-path (*configuration-directory*)
				 (*configuration-filename*)))
	  (config `(pegasus (repositories (,nickname ,repository)))))
      (unless (file-exists? (*configuration-directory*))
	(create-directory* (*configuration-directory*)))
      (when (file-exists? init-file) (delete-file init-file))
      (call-with-output-file init-file (cut write config <>))))

  (define (read-configuration)
    (let ((init-file (build-path (*configuration-directory*)
				 (*configuration-filename*))))
      (make-configuration (call-with-input-file init-file (cut read <>)))))

  ;; TODO macro?
  (define (work-directory)
    (let ((dir (build-path (*configuration-directory*) +work+)))
      (unless (file-exists? dir) (create-directory* dir))
      dir))

  (define (installed-directory)
    (let ((dir (build-path (*configuration-directory*) +installed+)))
      (unless (file-exists? dir) (create-directory dir))
      dir))

)


;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; pegasus/formula.scm - Formula resolution
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

(library (pegasus formula)
    (export read-formula
	    find-formula

	    find-formulas

	    formula-package

	    <homepage>
	    <author>
	    <repository> <source> <library-mixin>
	    <dependency>
	    <install>
	    <sagittarius-version>
	    <test>
	    <formula>
	    
	    )
    (import (rnrs)
	    (sagittarius)
	    (sagittarius object)
	    (clos user)
	    (pegasus repository)
	    (pegasus config)
	    (srfi :26 cut)
	    (util file))

  (define-class <name&version-mixin> ()
    ((name    :init-keyword :name)
     (version :init-keyword :version :init-value #f)))

  (define-class <homepage> ()
    ((url  :init-keyword :url :init-keyword "no project site")))

  (define-class <author> ()
    ((name  :init-keyword :name :init-value "No name")
     (email :init-keyword :email :init-value "no-email")))

  (define-class <library-mixin> ()
    ((type  :init-keyword :type)
     (url   :init-keyword :url)))

  (define-class <repository> (<library-mixin>)
    ;; version #f means HEAD
    ((version :init-keyword :version :init-value #f)))

  (define-class <source> (<library-mixin>)
    ((compression :init-keyword :compression)))

  (define-class <dependency> (<name&version-mixin>) ())

  (define-class <install> ()
    ((files :init-keyword :files :init-value '())
     (directories :init-keyword :directories :init-value '())))

  (define-class <sagittarius-version> ()
    ((minimum :init-keyword :minimum :init-value #f)
     (maximum :init-keyword :maximum :init-value #f)))

  (define-class <test> ()
    ((file :init-keyword :file)
     ;; default SRFI-64 :)
     (style :init-keyword :style :init-value 'srfi-64)
     (loadpath :init-keyword :loadpath :init-keyword ".")))

  (define-class <formula> (<name&version-mixin>)
    ((description  :init-keyword :description)
     (homepage     :init-keyword :homepage :init-value #f)
     (author       :init-keyword :author)
     (sagittarius-version :init-keyword :sagittarius-version :init-value #f)
     (dependencies :init-keyword :dependencies :init-value '())
     (repository   :init-keyword :repository :init-value #f)
     (source       :init-keyword :source :init-value #f)
     (install      :init-keyword :install)
     (tests        :init-keyword :tests :init-value '())))

  (define (make-dependencies deps)
    (map (lambda (d)
	   (unless (get-keyword :name d #f)
	     (error 'read-formula "dependency must have name" d))
	   (apply make <dependency> d))
	 (map cdr deps)))

  (define (make-install install)
    (let ((files (assq 'files install))
	  (directories (assq 'directories install)))
      (unless (or files directories)
	(error 'read-formula "install must have either files or directories"
	       install))
      (make <install> :files (or (and files (cdr files)) '())
	    :directories (or (and directories (cdr directories)) '()))))

  (define (read-formula name in)
    (let ((formula (read in)))
      (unless (eq? (car formula) 'formula)
	(error 'read-formula "not a formula" formula))
      (let* ((info (cdr formula))
	     (desc (assq 'description info))
	     (home (assq 'homepage info))
	     (auth (assq 'author info))
	     (ver  (assq 'version info))
	     (deps (assq 'dependencies info))
	     (repo (assq 'repository info))
	     (src  (assq 'source info))
	     (install (assq 'install info))
	     (tests (assq 'tests info))
	     (sg-ver  (assq 'sagittarius-version info)))
	(when (or (and src repo) (not (or src repo)))
	  (error 'read-formula "formula must have either repository or source"
		 formula))
	(unless ver (error 'read-formula "version is missing" formula))
	(unless install (error 'read-formula "install is missing" formula))
	(make <formula>
	  :name name
	  :description (and desc (cadr desc))
	  :homepage (and home (apply make <homepage> (cdr home)))
	  :version  (cadr ver)
	  :sagittarius-version (and sg-ver (apply make <sagittarius-version> 
						  (cdr sg-ver)))
	  :author   (and auth (apply make <author> (cdr auth)))
	  :dependencies (or (and deps (make-dependencies (cdr deps))) '())
	  :repository (and repo (apply make <repository> (cdr repo)))
	  :source   (and src (apply make <source> (cdr src)))
	  :install  (make-install (cdr install))
	  ;; don't check if the form is invalid ... should I?
	  :tests    (and tests (map (lambda (t) (apply make <test> (cdr t)))
				    (cdr tests)))))))

  (define (find-formula package)
    (let ((config (read-configuration)))
      (let loop ((reps (~ config 'repositories)))
	(if (null? reps)
	    #f
	    (let ((name (build-path (*configuration-directory*) (caar reps))))
	      (unless (file-directory? name)
		;; TODO init repository
		(error 'find-formula "not implemented yet"))
	      (let ((path (build-path* name +formula+
				       (string-append package ".scm"))))
		(if (file-exists? path)
		    (call-with-input-file path (cut read-formula package <>))
		    (loop (cdr reps)))))))))

  (define (formula-package formula)
    (cond ((~ formula 'repository)
	   => (lambda (r) (values 'repository r)))
	  (else (values 'source (~ formula 'source)))))

  (define (find-formulas :optional (pattern #f))
    (let ((config (read-configuration)))
      (let loop ((reps (~ config 'repositories)) (r '()))
	(if (null? reps)
	    r
	    (let ((files (find-files (build-path* (*configuration-directory*)
						  (caar reps)
						  +formula+)
				     :pattern (if pattern pattern #f))))
	      (loop (cdr reps) `(,@files ,@r)))))))

)

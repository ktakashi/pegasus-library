#!/bin/env sash
;; -*- mode:scheme; coding:utf-8; -*-
(import (rnrs) 
	(pegasus operations)
	(pegasus config)
	(clos user)
	(sagittarius control)
	(getopt)
	(util file)
	(srfi :1 lists)
	(srfi :26 cut))

  
;; TODO add options

(define (usage . args)
  (print "pegasus: [-s][-q] command command-options...")
  (print "  common options")
  (print "    -s, --silent  no verbose message")
  (print "    -q, --quiet   no prompt and install even if versions are different")
  (newline)
  (let1 commands (vector->list (all-commands))
    (print "  pegasus commands")
    (for-each (lambda (c) (print "    " c)) (map car commands))
    (print "for more detail, use help command.")
    (newline)
    ;;(for-each (lambda (h) (print h)) (map cdr commands))
    (exit -1)))

(define (main args)
  (unless (file-directory? (*configuration-directory*))
    (create-directory* (*configuration-directory*)))
  (when (null? (cdr args)) (usage args))
  (with-args args
      ((silent (#\s "silent") #f #f)
       (quiet  (#\q "quiet") #f #f)
       . rest)
    (let ((command (string->symbol (cadr rest)))
	  (args    (cddr rest)))
      (when quiet (*prompt* #f))
      (apply (lookup-command command usage) 
	     `(,@rest ,@(if silent '() '("-v")))))))

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

;; TODO implement user input prompt
(define (prompt) #t)
  
;; TODO add options

(define (usage . args)
  (let1 commands (vector->list (all-commands))
    (print "pegasus command")
    (for-each (lambda (c) (print "  " c)) (map car commands))
    (print "for more detail, use help command.")
    (newline)
    ;;(for-each (lambda (h) (print h)) (map cdr commands))
    (exit -1)))

(define (main args)
  (unless (file-directory? (*configuration-directory*))
    (create-directory* (*configuration-directory*)))
  (when (null? (cdr args)) (usage args))
  (let ((command (string->symbol (cadr args)))
	(args    (cddr args)))
    (*prompt* prompt)
    (with-args args
	((silent (#\s "silent") #f #f)
	 . rest)
      (apply (lookup-command command usage) 
	     `(,@rest ,@(if silent '() '("-v")))))))

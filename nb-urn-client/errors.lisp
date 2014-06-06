(in-package :nb-urn-client)

(defun trim-whitespace (str)
  (string-trim '(#\Space #\Newline #\Backspace #\Tab 
		 #\Linefeed #\Page #\Return #\Rubout)
	       str))

(define-condition nb-urn-client-error (error)
  ((what :initarg :what :initform "Something went wrong with the client." :reader what))
  (:report (lambda (condition s)
	     (format s "~A" (what condition))))
  (:documentation "Top level condition."))

(define-condition soap-fault-error (nb-urn-client-error)
  ((faultcode :initarg :faultcode :initform "" :reader faultcode)
   (faultstring :initarg :faultstring :initform "" :reader faultstring)
   (detail :initarg :detail :initform "" :reader detail))
  (:report (lambda (condition s)
	     (format s "Faultcode: ~A~%Faultstring: ~A~%Detail: ~A~%"
		     (trim-whitespace (faultcode condition))
		     (trim-whitespace (faultstring condition))
		     (trim-whitespace (detail condition))))))



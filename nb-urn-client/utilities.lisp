(in-package :nb-urn-client)

(defun trim-whitespace (str)
  (string-trim '(#\Space #\Newline #\Backspace #\Tab 
		 #\Linefeed #\Page #\Return #\Rubout)
	       str))

(defun null-or-empty-string-p (str)
  (or (null str) (equal "" (trim-whitespace str))))

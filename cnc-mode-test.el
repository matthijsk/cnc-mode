(require 'cnc-mode)

(ert-deftest cnc-test-number-of-digits ()
  (should (eql (cnc-number-of-digits 0) 1))
  (should (eql (cnc-number-of-digits 1) 1))
  (should (eql (cnc-number-of-digits 2) 1))
  (should (eql (cnc-number-of-digits 10) 2))
  (should (eql (cnc-number-of-digits 5000) 4))
  (should (eql (cnc-number-of-digits 123456789) 9))

  (should (eql (cnc-number-of-digits -0) 1))
  (should (eql (cnc-number-of-digits -1) 1))
  (should (eql (cnc-number-of-digits -2) 1))
  (should (eql (cnc-number-of-digits -10) 2))
  (should (eql (cnc-number-of-digits -5000) 4))
  (should (eql (cnc-number-of-digits -123456789) 9))

  ;; Characters are integers.
  (should (eql (cnc-number-of-digits ?\a) 1))
  (should (eql (cnc-number-of-digits ?\d) 3))
  (should (eql (cnc-number-of-digits ?c) 2))

  (should-error (eql (cnc-number-of-digits 'wrong-type-argument) 'wrong-type-argument))
  (should-error (eql (cnc-number-of-digits "wrong type") 'wrong-type-argument))
  (should-error (eql (cnc-number-of-digits 1.0) 'wrong-type-argument))
  (should-error (eql (cnc-number-of-digits -1.0) 'wrong-type-argument)))

(ert-deftest cnc-test-remove-line-numbers ()
  (with-temp-buffer
    (dotimes (count 100)
      (insert (format "N%d G01\n" count)))
    (goto-char (point-min))
    (end-of-line)
    ;; verify lines start with a line number
    (should (equal (buffer-substring (point-min) (point)) "N0 G01"))
    (goto-char (1- (point-max)))
    (beginning-of-line)
    (cnc-remove-line-numbers)
    ;; verify lines don't start with a line number
    (should (equal (buffer-substring (point) (1- (point-max))) "G01"))))

(ert-deftest cnc-test-renumber-lines ()
  ;; Test with padding
  (let ((cnc-line-number-start 10)
        (cnc-line-number-increment 10)
        (cnc-line-number-padding t)
        (cnc-line-number-append-string " "))
    (with-temp-buffer
      (dotimes (count 100)
        (insert (format "G01\n" count)))
      (cnc-renumber-lines)
      (goto-char (point-min))
      (search-forward " ")
      (should (equal (buffer-substring (point-min) (point)) "N0010 "))
      (forward-line 49)
      (let ((start (point)))
        (search-forward " ")
        (should (equal (buffer-substring start (point)) "N0500 ")))))

  ;; Test with no padding
  (let ((cnc-line-number-start 10)
        (cnc-line-number-increment 10)
        (cnc-line-number-padding nil)
        (cnc-line-number-append-string " "))
    (with-temp-buffer
      (dotimes (count 100)
        (insert (format "G01\n" count)))
      (cnc-renumber-lines)
      (goto-char (point-min))
      (search-forward " ")
      (should (equal (buffer-substring (point-min) (point)) "N10 "))
      (forward-line 49)
      (let ((start (point)))
        (search-forward " ")
        (should (equal (buffer-substring start (point)) "N500 "))))))

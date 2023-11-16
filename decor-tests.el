;;; decor-tests.el -- tests for decor

;;; Code:

(require 'ert)
(require 'decor)

(ert-deftest decor-check-bin ()
  (with-temp-buffer
    (should (not (decor--check-bin (buffer-name (current-buffer)) "sh")))
    (should (string-equal "" (buffer-string))))
  (with-temp-buffer
    (let ((cmd "missing"))
      (should (decor--check-bin (buffer-name (current-buffer)) cmd))
      (should (string-equal
               (format "'%s' not found\n" cmd)
               (buffer-string))))))

(provide 'decor-tests)

;;; decor-tests.el ends here

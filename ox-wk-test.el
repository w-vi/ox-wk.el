;;; ox-wk-test.el --- org-mode wiki export tests

;;; Commentary:

;; Run standalone with this,
;;   emacs -batch -L . -l ox.wk-test.el -f ert-run-tests-batch

;;; Code:

(require 'ert)
(require 'org)
(require 'ox-wk)

(defun ox-wk-load-file-str (filepath)
  "Return FILEPATH file content as string."
  (with-temp-buffer
    (insert-file-contents filepath)
    (org-trim (buffer-string))))

(ert-deftest ox-wk-export ()
  "Test the validate function."
  (with-current-buffer (find-file-noselect "test/test.org")
    (let ((code (org-export-as 'wk)))
      (should
       (string-equal
        (ox-wk-load-file-str "test.txt")
        (org-trim code))))))

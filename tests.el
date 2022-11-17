#!/usr/bin/env -S emacs --script

(add-to-list 'load-path default-directory)

(require 'ert)
(require 'wgetpaste)

(ert-deftest test-list-services ()
  (should (equal (wgetpaste-list-services)
                 '("0x0"
                   "bpaste"
                   "codepad"
                   "dpaste"
                   "gists"
                   "ix_io"
                   "snippets"
                   "sprunge"))))

(ert-deftest test-list-expirations ()
  (should (equal (wgetpaste-list-expirations "bpaste")
                 '("1day"
                   "1week"
                   "1month"
                   "never"))))

(ert-deftest test-list-languages ()
  (should (equal (wgetpaste-list-languages "dpaste")
                 '("Apache Config"
                   "Bash"
                   "CSS"
                   "Diff"
                   "Django Template/HTML"
                   "Haskell"
                   "JavaScript"
                   "Plain Text"
                   "Python"
                   "Python Interactive/Traceback"
                   "Ruby"
                   "Ruby HTML (ERB)"
                   "SQL"
                   "XML"))))

(ert-run-tests-batch-and-exit)

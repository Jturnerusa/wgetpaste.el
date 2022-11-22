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

(ert-deftest test-build-args ()
  (should (equal (wgetpaste-build-args
                  :language "bpaste"
                  :nick "emacs"
                  :description "an example paste"
                  :expiration "1day"
                  :ignore-configs)
                 '("--language" "bpaste"
                   "--nick" "emacs"
                   "--description" "an example paste"
                   "--expiration" "1day"
                   "--ignore-configs"))))

(ert-deftest test-parse-url ()
  (should (equal (wgetpaste-parse-url-from-process-output
                  "Your paste can be seen here: https://dpaste.com/testpaste\n")
                 "https://dpaste.com/testpaste")))

(ert-run-tests-batch-and-exit)

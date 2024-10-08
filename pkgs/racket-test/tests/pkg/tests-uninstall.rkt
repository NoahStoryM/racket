#lang racket/base
(require rackunit
         racket/system
         racket/match
         (for-syntax racket/base
                     syntax/parse)
         racket/file
         racket/runtime-path
         racket/path
         racket/list
         "shelly.rkt"
         "util.rkt")

(this-test-is-run-by-the-main-test)

(pkg-tests
 (shelly-begin
  (initialize-catalogs)
  
  (shelly-case
   "uninstall and show"
   (shelly-case "uninstall of not installed package fails"
                $ "raco pkg show -l -u -a" =stdout> " [none]\n"
                $ "raco pkg uninstall not-there" =exit> 1
                $ "raco pkg uninstall --dry-run not-there" =exit> 1)
   (shelly-case "uninstall of bad name"
                $ "raco pkg uninstall bad/" =exit> 1
                =stderr> #rx"disallowed")
   (shelly-case "uninstall of bad name"
                $ "raco pkg uninstall bad#2" =exit> 1
                =stderr> #rx"disallowed")
   (shelly-install "uninstall test"
                   "test-pkgs/pkg-test1.zip")
   (with-fake-root
     (shelly-case
      "Test `remove` alias"
      $ "racket -e '(require pkg-test1)'" =exit> 1
      $ "raco pkg install --copy test-pkgs/pkg-test1.zip"
      $ "racket -e '(require pkg-test1)'"
      $ "raco pkg remove pkg-test1"
      $ "racket -e '(require pkg-test1)'" =exit> 1))
   (shelly-install "uninstall test with immediately redundant package name"
                   "test-pkgs/pkg-test1.zip"
                   "pkg-test1 pkg-test1")
   (shelly-install "uninstall test where second in given list does not exist"
                   "test-pkgs/pkg-test1.zip"
                   $ "raco pkg uninstall pkg-test1 not-there" =exit> 1 =stderr> #rx"not currently installed"
                   $ "racket -e '(require pkg-test1)'" =exit> 0)
   (shelly-install "uninstall of dep fails"
                   "test-pkgs/pkg-test1.zip"
                   $ "raco pkg show -l -u -a" =stdout> #rx"Package +Checksum +Source\npkg-test1 +[a-f0-9.]+ +\\(file .+/test-pkgs/pkg-test1.zip\"\\)\n"
                   $ "raco pkg install test-pkgs/pkg-test2.zip"
                   $ "raco pkg show -l -u -a" =stdout> #rx"Package +Checksum +Source\npkg-test1 +[a-f0-9.]+ +\\(file .+/test-pkgs/pkg-test1.zip\"\\)\npkg-test2 +[a-f0-9.]+ +\\(file .+/test-pkgs/pkg-test2.zip\"\\)\n"
                   $ "raco pkg uninstall pkg-test1" =exit> 1 =stderr> #rx"pkg-test1 \\(required by: \\(pkg-test2\\)\\)"
                   $ "raco pkg uninstall --dry-run pkg-test1" =exit> 1 =stderr> #rx"pkg-test1 \\(required by: \\(pkg-test2\\)\\)"
                   $ "raco pkg uninstall --dry-run pkg-test2"
                   $ "raco pkg show -l -u -a" =stdout> #rx"Package +Checksum +Source\npkg-test1 +[a-f0-9.]+ +\\(file .+/test-pkgs/pkg-test1.zip\"\\)\npkg-test2 +[a-f0-9.]+ +\\(file .+/test-pkgs/pkg-test2.zip\"\\)\n"
                   $ "raco pkg uninstall pkg-test2"
                   $ "raco pkg show -l -u -a" =stdout>  #rx"Package +Checksum +Source\npkg-test1 +[a-f0-9.]+ +\\(file .+/test-pkgs/pkg-test1.zip\"\\)\n")
   (shelly-install "uninstall of dep can be forced"
                   "test-pkgs/pkg-test1.zip"
                   $ "raco pkg install test-pkgs/pkg-test2.zip"
                   $ "racket -e '(require pkg-test2/contains-dep)'" =exit> 0
                   $ "raco pkg uninstall --dry-run --force pkg-test1"
                   $ "racket -e '(require pkg-test2/contains-dep)'" =exit> 0
                   $ "raco pkg uninstall --force pkg-test1"
                   $ "racket -e '(require pkg-test2/contains-dep)'" =exit> 1
                   $ "raco pkg install test-pkgs/pkg-test1.zip"
                   $ "raco pkg uninstall pkg-test2")
   (with-fake-root
    (shelly-case
     "uninstall two"
     $ "racket -e '(require pkg-test1)'" =exit> 1
     $ "racket -e '(require pkg-test2)'" =exit> 1
     $ "raco pkg install test-pkgs/pkg-test2.zip test-pkgs/pkg-test1.zip" =exit> 0
     $ "racket -e '(require pkg-test1)'" =exit> 0
     $ "racket -e '(require pkg-test2)'" =exit> 0
     $ "racket -e '(require pkg-test2/contains-dep)'" =exit> 0
     $ "raco pkg uninstall pkg-test1 pkg-test2"
     $ "racket -e '(require pkg-test1)'" =exit> 1
     $ "racket -e '(require pkg-test2)'" =exit> 1))
   (with-fake-root 
    (shelly-case
     "uninstall two leaves no auto-uninstall suggestions"
     $ "raco pkg config --set catalogs http://localhost:9990"
     $ "racket -e '(require pkg-test1)'" =exit> 1
     $ "racket -e '(require pkg-test2)'" =exit> 1
     $ "raco pkg install --deps search-auto test-pkgs/pkg-test2.zip" =exit> 0
     $ "raco pkg show -l -u -a" =stdout> #rx"Package\\[\\*=auto\\] +Checksum +Source\npkg-test1\\* +[a-f0-9.]+ +\\(catalog \"pkg-test1\"\\)\npkg-test2 +[a-f0-9.]+ +\\(file .+/test-pkgs/pkg-test2.zip\"\\)\n"
     $ "racket -e '(require pkg-test1)'" =exit> 0
     $ "racket -e '(require pkg-test2)'" =exit> 0
     $ "racket -e '(require pkg-test2/contains-dep)'" =exit> 0
     $ "raco pkg uninstall pkg-test2 pkg-test1" =stdout> #rx"(?!no longer)"
     $ "raco pkg show -l -u -a" =stdout> " [none]\n"
     $ "racket -e '(require pkg-test1)'" =exit> 1
     $ "racket -e '(require pkg-test2)'" =exit> 1)
    (shelly-case
     "auto-uninstall"
     $ "raco pkg config --set catalogs http://localhost:9990"
     $ "racket -e '(require pkg-test1)'" =exit> 1
     $ "racket -e '(require pkg-test2)'" =exit> 1
     $ "raco pkg install --deps search-auto test-pkgs/pkg-test2.zip" =exit> 0
     $ "raco pkg show -l -u -a" =stdout> #rx"Package\\[\\*=auto\\] +Checksum +Source\npkg-test1\\* +[a-f0-9.]+ +\\(catalog \"pkg-test1\"\\)\npkg-test2 +[a-f0-9.]+ +\\(file .+/test-pkgs/pkg-test2.zip\"\\)\n"
     $ "racket -e '(require pkg-test1)'" =exit> 0
     $ "racket -e '(require pkg-test2)'" =exit> 0
     $ "racket -e '(require pkg-test2/contains-dep)'" =exit> 0
     $ "raco pkg uninstall pkg-test2" =stdout> #px"automatically installed.*no longer[^:]+:\n\\s+pkg-test1\n[^\n]+raco pkg uninstall --auto"
     $ "raco pkg show -l -u -a" =stdout> #rx"Package\\[\\*=auto\\] +Checksum +Source\npkg-test1\\* +[a-f0-9.]+ +\\(catalog \"pkg-test1\"\\)\n"
     $ "racket -e '(require pkg-test1)'" =exit> 0
     $ "raco pkg uninstall --auto"
     $ "raco pkg show -l -u -a" =stdout> " [none]\n"
     $ "racket -e '(require pkg-test1)'" =exit> 1
     $ "racket -e '(require pkg-test2)'" =exit> 1)
    (shelly-case
     "demote rather than uninstall"
     $ "raco pkg config --set catalogs http://localhost:9990"
     $ "racket -e '(require pkg-test1)'" =exit> 1
     $ "racket -e '(require pkg-test2)'" =exit> 1
     $ "raco pkg install --deps search-auto test-pkgs/pkg-test2.zip" =exit> 0
     $ "raco pkg show -l -u -a" =stdout> #rx"Package\\[\\*=auto\\] +Checksum +Source\npkg-test1\\* +[a-f0-9.]+ +\\(catalog \"pkg-test1\"\\)\npkg-test2 +[a-f0-9.]+ +\\(file .+/test-pkgs/pkg-test2.zip\"\\)\n"
     $ "raco pkg uninstall --demote pkg-test2" =stdout> #px"automatically installed.*no longer[^:]+:\n\\s+pkg-test1\\s+pkg-test2\n[^\n]+raco pkg uninstall --auto"
     $ "raco pkg show -l -u -a" =stdout> #rx"Package\\[\\*=auto\\] +Checksum +Source\npkg-test1\\* +[a-f0-9.]+ +\\(catalog \"pkg-test1\"\\)\npkg-test2\\* +[a-f0-9.]+ +\\(file .+/test-pkgs/pkg-test2.zip\"\\)\n"
     $ "racket -e '(require pkg-test1)'" =exit> 0
     $ "racket -e '(require pkg-test2)'" =exit> 0
     $ "racket -e '(require pkg-test2/contains-dep)'" =exit> 0
     $ "raco pkg uninstall --auto"
     $ "raco pkg show -l -u -a" =stdout> " [none]\n"
     $ "racket -e '(require pkg-test1)'" =exit> 1
     $ "racket -e '(require pkg-test2)'" =exit> 1)
     (shelly-case
     "demote, but cannot auto-uninstall"
     $ "racket -e '(require pkg-test1)'" =exit> 1
     $ "racket -e '(require pkg-test2)'" =exit> 1
     $ "raco pkg install test-pkgs/pkg-test2.zip test-pkgs/pkg-test1.zip" =exit> 0
     $ "raco pkg show -l -u -a" =stdout> #rx"Package +Checksum +Source\npkg-test1 +[a-f0-9.]+ +\\(file .+/test-pkgs/pkg-test1.zip\"\\)\npkg-test2 +[a-f0-9.]+ +\\(file .+/test-pkgs/pkg-test2.zip\"\\)\n"
     $ "raco pkg uninstall --demote pkg-test1" =stdout> #rx"(?!no longer)"
     $ "raco pkg show -l -u -a" =stdout> #rx"Package\\[\\*=auto\\] +Checksum +Source\npkg-test1\\* +[a-f0-9.]+ +\\(file .+/test-pkgs/pkg-test1.zip\"\\)\npkg-test2 +[a-f0-9.]+ +\\(file .+/test-pkgs/pkg-test2.zip\"\\)\n"
     $ "racket -e '(require pkg-test1)'" =exit> 0
     $ "racket -e '(require pkg-test2)'" =exit> 0
     $ "racket -e '(require pkg-test2/contains-dep)'" =exit> 0
     $ "raco pkg uninstall --auto pkg-test2"
     $ "raco pkg show -l -u -a" =stdout> " [none]\n"
     $ "racket -e '(require pkg-test1)'" =exit> 1
     $ "racket -e '(require pkg-test2)'" =exit> 1)
    (shelly-case
     "single-step auto-uninstall"
     $ "raco pkg install --deps search-auto test-pkgs/pkg-test2.zip" =exit> 0
     $ "raco pkg uninstall --auto pkg-test2" =stdout> #rx"(?!no longer)"
     $ "raco pkg show -l -u -a" =stdout> " [none]\n"
     $ "racket -e '(require pkg-test1)'" =exit> 1
     $ "racket -e '(require pkg-test2)'" =exit> 1)
    (shelly-case
     "single-step auto-uninstall with cycles"
     $ "raco pkg install --deps search-auto --copy test-pkgs/pkg-cycle1" =exit> 0
     $ "racket -e '(require pkg-cycle1)'" =exit> 0
     $ "racket -e '(require pkg-cycle2)'" =exit> 0
     $ "raco pkg uninstall --auto pkg-cycle1"
     $ "raco pkg show -l -u -a" =stdout> " [none]\n"
     $ "racket -e '(require pkg-cycle1)'" =exit> 1
     $ "racket -e '(require pkg-cycle2)'" =exit> 1))
   (with-fake-root
    (shelly-case
     "different scope error"
     $ "raco pkg install test-pkgs/pkg-test1.zip" =exit> 0
     $ "raco pkg uninstall --installation pkg-test1" =exit> 1
     =stderr> #rx"package installed in a different scope"
     $ "raco pkg uninstall pkg-test1")))))

#lang info

(define collection 'multi)

(define deps '("base"))

(define pkg-desc "tests for \"net-lib\"")

(define pkg-authors '(mflatt))
(define build-deps '("at-exp-lib"
                     "compatibility-lib"
                     "eli-tester"
                     "net-lib"
                     "racket-test"
                     "rackunit-lib"
                     "sandbox-lib"
                     "web-server-lib"))
(define update-implies '("net-lib"))

(define license
  '(Apache-2.0 OR MIT))

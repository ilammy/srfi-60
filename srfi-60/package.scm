(package
  (name (srfi 60))
  (description "SRFI 60 'Integers as Bits'")
  (homepage "https://github.com/ilammy/srfi-60")
  (authors "ilammy")
  (library
    (name (srfi 60))
    (path "srfi/60.sld")
    (depends
      (scheme base)
      (scheme case-lambda))
    (test-program "test/srfi-60-tests.scm")
    (test-program "test/generated/srfi-60-generated-4.scm")
    (test-program "test/generated/srfi-60-generated-8.scm")
    (test-program "test/generated/srfi-60-generated-16.scm")
    (test-program "test/generated/srfi-60-generated-32.scm")
    (test-program "test/generated/srfi-60-generated-256.scm")
    (test-program "test/generated/srfi-60-generated-1024.scm"))
  (program
    (use-for test)
    (path "test/srfi-60-tests.scm")
    (depends
      (chibi)
      (chibi test)
      (srfi 60)))
  (program
    (use-for test)
    (path "test/generated/srfi-60-generated-4.scm")
    (depends
      (chibi)
      (chibi test)
      (srfi 60)))
  (program
    (use-for test)
    (path "test/generated/srfi-60-generated-8.scm")
    (depends
      (chibi)
      (chibi test)
      (srfi 60)))
  (program
    (use-for test)
    (path "test/generated/srfi-60-generated-16.scm")
    (depends
      (chibi)
      (chibi test)
      (srfi 60)))
  (program
    (use-for test)
    (path "test/generated/srfi-60-generated-32.scm")
    (depends
      (chibi)
      (chibi test)
      (srfi 60)))
  (program
    (use-for test)
    (path "test/generated/srfi-60-generated-256.scm")
    (depends
      (chibi)
      (chibi test)
      (srfi 60)))
  (program
    (use-for test)
    (path "test/generated/srfi-60-generated-1024.scm")
    (depends
      (chibi)
      (chibi test)
      (srfi 60))))

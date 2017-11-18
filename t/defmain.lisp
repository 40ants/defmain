(in-package :cl-user)
(defpackage defmain-test
  (:use :cl
        :defmain
        :prove
        :hamcrest.prove))
(in-package :defmain-test)


(plan 1)

(subtest "Checking if map-fields calls function for each fields and adds a help field."
  (subtest "Simple case, when only one field is specified"
    (let* ((fields '((environment "The environment name")))
           (result  (defmain::map-fields (lambda (&rest args) args)
                                         fields)))
      (assert-that result
                   (contains '(help "Show help on this program." :flag t)
                             '(environment "The environment name")))))

  (subtest "In case if &rest parameter is present, it should be ignored."
    (let* ((fields '((environment "The environment name")
                     &rest some))
           (result  (defmain::map-fields (lambda (&rest args) args)
                                         fields)))
      (assert-that result
                   (contains '(help "Show help on this program." :flag t)
                             '(environment "The environment name"))))))

(finalize)

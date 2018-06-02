(defpackage defmain-test
  (:use #:cl
        #:defmain
        #:rove
        #:hamcrest/rove)
  (:import-from #:defmain
                #:help))
(in-package :defmain-test)


(deftest test-map-fields
    (testing "Checking if map-fields calls function for each fields and adds a help field."
             (testing "Simple case, when only one field is specified"
                      (let* ((fields '((environment "The environment name")))
                             (result  (defmain::map-fields (lambda (&rest args) args)
                                                           fields)))
                        (assert-that result
                                     (contains '(help "Show help on this program." :flag t)
                                               '(environment "The environment name")))))

             (testing "In case if &rest parameter is present, it should be ignored."
                      (let* ((fields '((environment "The environment name")
                                       &rest some))
                             (result  (defmain::map-fields (lambda (&rest args) args)
                                                           fields)))
                        (assert-that result
                                     (contains '(help "Show help on this program." :flag t)
                                               '(environment "The environment name")))))))

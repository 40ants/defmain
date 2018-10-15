(defpackage defmain-test
  (:use #:cl
        #:defmain
        #:rove
        #:hamcrest/rove)
  (:import-from #:defmain
                #:help))
(in-package defmain-test)


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

(deftest test-make-synopsis-fields
  (testing "If there are fields without documentation, they should be considered as positional and skipped here"
    (let* ((fields '(user
                     dbname
                     (environment "The environment name")
                     &rest some))
           (result  (defmain::make-synopsis-fields fields)))
      (assert-that result
                   (contains `(defmain/defmain::flag :long-name "help"
                                                     :env-var nil
                                                     :description "Show help on this program."
                                                     :short-name "h")
                             `(defmain/defmain::stropt :long-name "environment"
                                                       :env-var nil
                                                       :description "The environment name"
                                                       :short-name "e"))))))

(deftest test-make-synopsis-args
  (testing "If empty input is OK"
    (ok (null (defmain::make-synopsis-args '()))))
  
  (testing "How &rest argument is transformed into the :postfix."
    (assert-that (defmain::make-synopsis-args '(&rest repositories))
                 (contains :postfix "REPOSITORY...")))
  
  (testing "How &subcommand argument is transformed into the :postfix."
    (assert-that (defmain::make-synopsis-args '(&subcommand))
                 (contains :postfix "COMMAND")))
  
  (testing "Arguments &rest and &subcommand can't be used together."
    (ok (signals (defmain::make-synopsis-args '(&subcommand &rest args))))))


(deftest test-is-has-subcommand
  (ok (equal (defmain::is-has-subcommand '((name "Your name") &subcommand))
             t))
  (ok (null (defmain::is-has-subcommand '((name "Your name"))))))


(deftest test-add-help-field
    (testing "It adds 'help symbol if it is not already exists."
             (assert-that (defmain::add-help-fields '((user "The username")))
                          (contains '(help "Show help on this program." :flag t)
                                    '(user "The username"))))
  
  (testing "It keeps 'help symbol if it exists."
           (assert-that (defmain::add-help-fields '((user "Имя пользователя")
                                                    (help "Показать справку" :flag t)))
                        (contains '(user "Имя пользователя")
                                  '(help "Показать справку" :flag t))))
  
  (testing "It adds help-commands if &subcommand is present"
           (assert-that (defmain::add-help-fields '((user "Имя пользователя")
                                                    &subcommand))
                        (contains '(defmain::help-commands "Show a list of all supported commands." :flag t :short nil)
                                  '(help "Show help on this program." :flag t)
                                  '(user "Имя пользователя")
                                  '&subcommand)))
  
  (testing "It keeps existing help-commands if &subcommand and help-commands are present."
           (assert-that (defmain::add-help-fields '((help-commands "Показать список команд" :flag t)
                                                    (user "Имя пользователя")
                                                    &subcommand))
                        (contains '(help "Show help on this program." :flag t)
                                  '(help-commands "Показать список команд" :flag t)
                                  '(user "Имя пользователя")
                                  '&subcommand))))


(deftest test-get-positional-args
  (testing "Options and &rest argument are ignored"
    (assert-that (defmain::get-positional-args '(user (pass "The password") &rest foo))
                 (contains 'user))))


(deftest test-postfix-string
  (testing "If \"rest\" argument given and has S at the end, it should be replaced with ..."
    (ok (string= (defmain::make-postfix-string nil 'cats)
                 "CAT...")
        (string= (defmain::make-postfix-string nil 'repositories)
                 "REPOSITORY..."))))


(ns avi.assert)

(defmacro assert-equal
  [[ignored-operator expected-value expression]]
  `(let [expected-value# ~expected-value
         expression# ~expression
         message# (str "Expected "
                       (pr-str expected-value#)
                       ", but got "
                       (pr-str expression#)
                       " for "
                       (pr-str '~expression))]
     (if-not (= expected-value# expression#)
       (throw (AssertionError. message#)))))

(ns avi.assert)

(defmacro assert-equal
  [[operator expected-value expression]]
  `(let [operator# ~operator
         expected-value# ~expected-value
         expression# ~expression
         message# (str "Expected "
                       (pr-str expected-value#)
                       ", but got "
                       (pr-str expression#)
                       " for "
                       (pr-str '~expression))]
     (if-not (operator# expected-value# expression#)
       (throw (AssertionError. message#)))))

(ns avi.assert)

(defmacro assert-equal
  [[comparison expected-value expression]]
  `(let [comparison# ~comparison
         expected-value# ~expected-value
         expression# ~expression
         message# (str "Expected "
                       (pr-str expected-value#)
                       ", but got "
                       (pr-str expression#)
                       " for "
                       (pr-str '~expression))]
     (if-not (comparison# expected-value# expression#)
       (throw (AssertionError. message#)))))

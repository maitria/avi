(ns avi.t-panes
  (:require [midje.sweet :refer :all]
            [avi.panes :as p]))

(facts "about finding panes to render"
  (fact "a single root pane fills the screen"
    (p/panes-to-render {:viewport {:size [10 8]}
                        :panes 0}) => [{:lens 0
                                        :offset [0 0]
                                        :size [9 8]}]
    (p/panes-to-render {:viewport {:size [8 17]}
                        :panes 6}) => [{:lens 6
                                        :offset [0 0]
                                        :size [7 17]}])
  (fact "a single horizonal split works correctly"
    (p/panes-to-render {:viewport {:size [10 8]}
                        :panes [:h 0 3 1]}) => [{:lens 0
                                                 :offset [0 0]
                                                 :size [3 8]}
                                                {:lens 1
                                                 :offset [3 0]
                                                 :size [6 8]}]))

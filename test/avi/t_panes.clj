(ns avi.t-panes
  (:require [midje.sweet :refer :all]
            [avi.panes :as p]))

(facts "about finding panes to render"
  (fact "a single root pane fills the screen"
    (p/panes-to-render {:viewport {:size [10 8]}
                        ::p/tree {::p/lens 0}}) => [{::p/lens 0
                                        :offset [0 0]
                                        :size [9 8]}]
    (p/panes-to-render {:viewport {:size [8 17]}
                        ::p/tree {::p/lens 6}}) => [{::p/lens 6
                                        :offset [0 0]
                                        :size [7 17]}])
  (fact "a single horizonal split works correctly"
    (p/panes-to-render {:viewport {:size [10 8]}
                        ::p/tree {::p/subtrees [{::p/lens 0 ::p/extent 3}
                                                {::p/lens 1}]}})
      => [{::p/lens 0
           :offset [0 0]
           :size [3 8]}
          {::p/lens 1
           :offset [3 0]
           :size [6 8]}])
  (fact "two horizontal splits work correctly"
    (p/panes-to-render {:viewport {:size [10 8]}
                        ::p/tree {::p/subtrees [{::p/lens 0 ::p/extent 2}
                                                {::p/lens 1 ::p/extent 2}
                                                {::p/lens 2}]}})
      => [{::p/lens 0
           :offset [0 0]
           :size [2 8]}
          {::p/lens 1
           :offset [2 0]
           :size [2 8]}
          {::p/lens 2
           :offset [4 0]
           :size [5 8]}]))

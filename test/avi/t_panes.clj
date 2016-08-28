(ns avi.t-panes
  (:require [midje.sweet :refer :all]
            [avi.layout :as layout]
            [avi.layout.panes :as p]))

(defn all-panes
  [tree]
  (sequence p/all-panes (p/augmented-root-panes tree)))

(facts "about finding panes to render"
  (fact "a single root pane fills the screen"
    (all-panes {::layout/shape [[0 0] [10 8]]
                ::p/tree {::p/lens 0}})
    => (contains (contains {::p/lens 0
                            ::layout/shape [[0 0] [9 8]]
                            ::p/path []}))
    (all-panes {::layout/shape [[0 0] [8 17]]
                ::p/tree {::p/lens 6}})
    => (contains (contains {::p/lens 6
                            ::layout/shape [[0 0] [7 17]]
                            ::p/path []})))
  (fact "a single horizonal split works correctly"
        (all-panes {::layout/shape [[0 0] [10 8]]
                    ::p/tree {::p/subtrees [{::p/lens 0 ::p/extent 3}
                                            {::p/lens 1}]}})
        => (contains (contains {::p/lens 0
                                ::layout/shape [[0 0] [3 8]]
                                ::p/extent 3
                                ::p/path [0]})
                     (contains {::p/lens 1
                                ::layout/shape [[3 0] [6 8]]
                                ::p/path [1]})))
  (fact "two horizontal splits work correctly"
    (all-panes {::layout/shape [[0 0] [10 8]]
                ::p/tree {::p/subtrees [{::p/lens 0 ::p/extent 2}
                                        {::p/lens 1 ::p/extent 2}
                                        {::p/lens 2}]}})
    => (contains (contains {::p/lens 0
                            ::layout/shape [[0 0] [2 8]]
                            ::p/extent 2
                            ::p/path [0]})
                 (contains {::p/lens 1
                            ::layout/shape [[2 0] [2 8]]
                            ::p/extent 2
                            ::p/path [1]})
                 (contains {::p/lens 2
                            ::layout/shape [[4 0] [5 8]]
                            ::p/path [2]}))))

(ns avi.normal-test
  (:require [midje.sweet :refer :all]
            [avi.normal :refer [ctrl]]
            [avi.test-helpers :refer :all]))

(facts "regarding repeating commands"
  (fact "`1` through `9` can be used as repeat counts."
    (cursor :editing "0123456789x" :after "1l") => [0 1]
    (cursor :editing "0123456789x" :after "2l") => [0 2]
    (cursor :editing "0123456789x" :after "3l") => [0 3]
    (cursor :editing "0123456789x" :after "4l") => [0 4]
    (cursor :editing "0123456789x" :after "5l") => [0 5]
    (cursor :editing "0123456789x" :after "6l") => [0 6]
    (cursor :editing "0123456789x" :after "7l") => [0 7]
    (cursor :editing "0123456789x" :after "8l") => [0 8]
    (cursor :editing "0123456789x" :after "9l") => [0 9])
  (fact "Multiple digits can be used as the repeat count."
    (cursor :editing "0000000000111111111" :after "17l") => [0 17])
  (fact "`0` can be used in a repeat count."
    (cursor :editing "0000000000111111111" :after "10l") => [0 10])
  (fact "The repeat goes away after a command is executed."
    (cursor :editing "0123456789x" :after "4ll") => [0 5])
  (fact "None of the digits clear the repeat count."
    (:count (editor :after "1234567890")) => 1234567890))

(facts "regarding cursor movement"
  (fact "The cursor starts on line 1, column 0."
    (cursor) => [0 0])

  (facts "about `j`"
    (fact "`j` moves the cursor down one line."
      (cursor :after "j") => [1 0]
      (cursor :after "jj") => [2 0])
    (fact "`j` can move to a zero-length line."
      (cursor :editing "One\n\nTwo" :after "j") => [1 0])
    (fact "`j` won't move the cursor below the last line."
      (cursor :after "jjjj") => [3 0]
      (editor :after "jjjj") => beeped)
    (fact "`j` won't place the cursor after the end of the line."
      (cursor :editing "Hello\nOne" :after "llllj") => [1 2])
    (fact "`j` remembers the last explicitly-set column."
      (cursor :editing "Hello\n.\nThere" :after "lljj") => [2 2]))

  (facts "about `k`"
    (fact "`k` moves the cursor up one line."
      (cursor :after "jk") => [0 0]
      (cursor :after "jjjkk") => [1 0])
    (fact "`k` won't move above the first line."
      (cursor :after "k") => [0 0]
      (editor :after "k") => beeped
      (editor :after "kj") => did-not-beep)
    (fact "`k` can move to a zero-length line."
      (cursor :editing "\nOne" :after "jk") => [0 0])
    (fact "`k` won't place the cursor after the end of the line."
      (cursor :editing "One\nHello" :after "jllllk") => [0 2]))

  (facts "about `l`"
    (fact "`l` moves to the right one character."
      (cursor :after "l") => [0 1]
      (cursor :after "ll") => [0 2])
    (fact "`l` won't move beyond the end of the line."
      (cursor :after "lll") => [0 2]
      (editor :after "lll") => beeped))

  (facts "about `h`"
    (fact "`h` moves to the left one character."
      (cursor :after "llh") => [0 1])
    (fact "`h` won't move before the beginning of the line."
      (cursor :after "h") => [0 0]
      (editor :after "h") => beeped))

  (facts "about moving to the beginning or end of line"
    (fact "`0` moves to the first character on the line."
      (cursor :after "ll0") => [0 0]
      (editor :editing "\n" :after "0") => did-not-beep)

    (fact "`$` moves to the last character on the line."
      (cursor :after "$") => [0 2])
    (fact "`^` moves to the first non-space character"
      (cursor :editing "bob" :after "$^") => [0 0]
      (cursor :editing "  bob" :after "$^") => [0 2]
      (editor :editing ".\n\n." :after "j^") => did-not-beep
      (cursor :editing "   " :after "0^") => [0 2]))

  (facts "about `G`"
    (fact "`G` moves to the last line."
      (cursor :editing ".\n.\nThree" :after "G") => [2 0])
    (fact "`G` moves to the line in the count register."
      (cursor :editing ".\n.\nThree" :after "2G") => [1 0])))

(facts "regarding scrolling"
  (fact "line-wise cursor movement will keep the cursor in the viewport"
    (fact "can scroll down some lines"
      (editor :editing ten-lines :after "7j")
       => (looks-like
            "Three               "
            "Four                "
            "Five                "
            "Six                 "
            "Seven               "
            "Eight               "
            "test.txt            " [:black :on :white]
            "                    ")
      (cursor :editing ten-lines :after "7j") => [5 0])
        
    (fact "viewport stays when moving back up"
      (editor :editing ten-lines :after "7jk")
       => (looks-like
            "Three               "
            "Four                "
            "Five                "
            "Six                 "
            "Seven               "
            "Eight               "
            "test.txt            " [:black :on :white]
            "                    ")
      (cursor :editing ten-lines :after "7jk") => [4 0])

    (fact "can scroll up some lines"
      (editor :editing ten-lines :after "7j6k")
       => (looks-like
            "Two                 "
            "Three               "
            "Four                "
            "Five                "
            "Six                 "
            "Seven               "
            "test.txt            " [:black :on :white]
            "                    ")
      (cursor :editing ten-lines :after "7j6k") => [0 0]))
  (facts "about `^E`"
    (fact "`^E` scrolls the buffer down one line"
      (editor :editing ten-lines :after [(ctrl \E)])
       => (looks-like
            "Two                 "
            "Three               "
            "Four                "
            "Five                "
            "Six                 "
            "Seven               "
            "test.txt            " [:black :on :white]
            "                    "))
    (fact "`^E` moves the cursor down to keep it in the viewport"
      (cursor :editing ten-lines :after [(ctrl \E)]) => [0 0])
    (fact "`^E` doesn't move the cursor when unnecessary"
      (cursor :editing ten-lines :after [\j \j (ctrl \E)]) => [1 0])
    (fact "`^E` won't put the cursor past end-of-line"
      (cursor :editing ten-lines :after [\3 \G \$ \3 (ctrl \E)]) => [0 3]))
  (facts "about `^Y`"
    (fact "`^Y` scrolls the buffer up one line"
      (editor :editing ten-lines :after [(ctrl \E) (ctrl \Y)])
       => (looks-like
             "One                 "
             "Two                 "
             "Three               "
             "Four                "
             "Five                "
             "Six                 "
             "test.txt            " [:black :on :white]
             "                    "))
    (fact "`^Y` moves the cursor up to keep it in the viewport"
      (cursor :editing ten-lines :after [\7 \G (ctrl \Y)]) => [5 0])
    (fact "`^Y` doesn't move the cursor when unnecessary"
      (cursor :editing ten-lines :after [(ctrl \E) (ctrl \Y)]) => [1 0])
    (fact "`^Y` won't put the cursor past end-of-line"
      (cursor :editing ten-lines :after [\7 \G \$ (ctrl \Y)]) => [5 2]))
  (facts "about `^D`"
    (fact "`^D` scrolls down half a page"
      (editor :editing ten-lines :after [(ctrl \D)])
       => (looks-like
             "Four                "
             "Five                "
             "Six                 "
             "Seven               "
             "Eight               "
             "Nine                "
             "test.txt            " [:black :on :white]
             "                    "))
    (fact "`^D` moves the cursor down half a page"
      (cursor :editing ten-lines :after [\j \j (ctrl \D)]) => [2 0])
    (fact "`^D` won't scroll past end-of-file"
      (editor :editing ten-lines :after [(ctrl \D) (ctrl \D) (ctrl \D)])
       => (looks-like
             "Five                "
             "Six                 "
             "Seven               "
             "Eight               "
             "Nine                "
             "Ten                 "
             "test.txt            " [:black :on :white]
             "                    "))
    (fact "`^D` near end-of-file moves the cursor to last line (and not past)"
      (cursor :editing ten-lines :after [\G \k (ctrl \D)]) => [5 0])
    (fact "`^D` on last line beeps"
      (editor :editing ten-lines :after [\G (ctrl \D)]) => beeped)
    (fact "`^D` won't scroll when file is shorter than screen"
      (editor :after [(ctrl \D)])
       => (looks-like
             "One                 "
             "Two                 "
             "Three               "
             ".                   "
             "~                   " [:blue]
             "~                   " [:blue]
             "test.txt            " [:black :on :white]
             "                    "))
    (fact "`^D` won't move cursor past end-of-file when file is shorter than screen"
      (cursor :editing "One\nTwo" :after [(ctrl \D)]) => [1 0]))
  (facts "about `^U`"
    (fact "`^U` on first line beeps"
      (editor :after [(ctrl \U)]) => beeped)
    (fact "`^U` scrolls up a half page"
      (editor :editing ten-lines :after [(ctrl \D) (ctrl \U)])
       => (looks-like
             "One                 "
             "Two                 "
             "Three               "
             "Four                "
             "Five                "
             "Six                 "
             "test.txt            " [:black :on :white]
             "                    "))))

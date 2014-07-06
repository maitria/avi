(ns avi.normal-mode-test
  (:require [midje.sweet :refer :all] 
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
    (fact "`G` moves to the first non-blank character on the last line"
      (cursor :editing "...\n...\n Three" :after "llG") => [2 1])
    (fact "`G` moves to the first non-blank character on the line in the count register."
      (cursor :editing "...\n ...\nThree" :after "ll2G") => [1 1])))

(facts "regarding scrolling"
  (fact "line-wise cursor movement will keep the cursor in the viewport"
    (fact "can scroll down some lines"
      (terminal :editing ten-lines :after "7j")
        => ["Three"
            "Four"
            "Five"
            "Six"
            "Seven"
            "Eight"
            "test.txt" :black :on :white
            ""]
      (cursor :editing ten-lines :after "7j") => [5 0])
        
    (fact "viewport stays when moving back up"
      (terminal :editing ten-lines :after "7jk")
        => ["Three"
            "Four"
            "Five"
            "Six"
            "Seven"
            "Eight"
            "test.txt" :black :on :white
            ""]
      (cursor :editing ten-lines :after "7jk") => [4 0])

    (fact "can scroll up some lines"
      (terminal :editing ten-lines :after "7j6k")
        => ["Two"
            "Three"
            "Four"
            "Five"
            "Six"
            "Seven"
            "test.txt" :black :on :white
            ""]
      (cursor :editing ten-lines :after "7j6k") => [0 0]))
  (facts "about `^E`"
    (fact "`^E` scrolls the buffer down one line"
      (terminal :editing ten-lines :after "<C-E>")
        => ["Two"
            "Three"
            "Four"
            "Five"
            "Six"
            "Seven"
            "test.txt" :black :on :white
            ""])
    (fact "`^E` moves the cursor down to keep it in the viewport"
      (cursor :editing ten-lines :after "<C-E>") => [0 0])
    (fact "`^E` doesn't move the cursor when unnecessary"
      (cursor :editing ten-lines :after "jj<C-E>") => [1 0])
    (fact "`^E` won't put the cursor past end-of-line"
      (cursor :editing ten-lines :after "3G$3<C-E>") => [0 3]))
  (facts "about `^Y`"
    (fact "`^Y` scrolls the buffer up one line"
      (terminal :editing ten-lines :after "<C-E><C-Y>")
        => ["One"
            "Two"
            "Three"
            "Four"
            "Five"
            "Six"
            "test.txt" :black :on :white
            ""])
    (fact "`^Y` moves the cursor up to keep it in the viewport"
      (cursor :editing ten-lines :after "7G<C-Y>") => [5 0])
    (fact "`^Y` doesn't move the cursor when unnecessary"
      (cursor :editing ten-lines :after "<C-E><C-Y>") => [1 0])
    (fact "`^Y` won't put the cursor past end-of-line"
      (cursor :editing ten-lines :after "7G$<C-Y>") => [5 2]))
  (facts "about `^D`"
    (fact "`^D` scrolls down half a page"
      (terminal :editing ten-lines :after "<C-D>")
        => ["Four"
            "Five"
            "Six"
            "Seven"
            "Eight"
            "Nine"
            "test.txt" :black :on :white
            ""])
    (fact "`^D` moves the cursor down half a page"
      (cursor :editing ten-lines :after "jj<C-D>") => [2 0])
    (fact "`^D` won't scroll past end-of-file"
      (terminal :editing ten-lines :after "<C-D><C-D><C-D>")
        => ["Five"
            "Six"
            "Seven"
            "Eight"
            "Nine"
            "Ten"
            "test.txt" :black :on :white
            ""])
    (fact "`^D` near end-of-file moves the cursor to last line (and not past)"
      (cursor :editing ten-lines :after "Gk<C-D>") => [5 0])
    (fact "`^D` on last line beeps"
      (editor :editing ten-lines :after "G<C-D>") => beeped)
    (fact "`^D` won't scroll when file is shorter than buffer viewport"
      (terminal :after "<C-D>")
        => ["One"
            "Two"
            "Three"
            "."
            "~" :blue
            "~" :blue
            "test.txt" :black :on :white
            ""])
    (fact "`^D` won't move cursor past end-of-file when file is shorter than buffer viewport"
      (cursor :editing "One\nTwo" :after "<C-D>") => [1 0]))

  (facts "about `^U`"
    (fact "`^U` on first line beeps"
      (editor :after "<C-U>") => beeped)
    (fact "`^U` scrolls up a half page"
      (terminal :editing ten-lines :after "<C-D><C-U>")
        => ["One"
            "Two"
            "Three"
            "Four"
            "Five"
            "Six"
            "test.txt" :black :on :white
            ""])
    (fact "`^U` moves the cursor up half a page"
      (cursor :editing ten-lines :after "Gk<C-U>") => [4 0])
    (fact "`^U` does not scroll to before first line of file"
      (terminal :editing ten-lines :after "<C-E><C-U>")
        => ["One"
            "Two"
            "Three"
            "Four"
            "Five"
            "Six"
            "test.txt" :black :on :white
            ""])
    (fact "`^U` does not move cursor before beginning of file"
      (cursor :editing ten-lines :after "j<C-U>") => [0 0])))

(facts "about navigating within the viewport"
  (facts "about `L`"
    (fact "`L` moves to the last line when buffer has fewer lines than the buffer viewport"
      (cursor :editing "One\nTwo\nThree" :after "L") => [2 0])
    (fact "`L` moves to the last line on the buffer viewport when the file is longer"
      (cursor :editing ten-lines :after "L") => [5 0]
      (terminal :editing ten-lines :after "L")
        => ["One"
            "Two"
            "Three"
            "Four"
            "Five"
            "Six"
            "test.txt" :black :on :white
            ""])
    (fact "`L` will move to count line from bottom of viewport"
      (cursor :editing ten-lines :after "<C-E>2L") => [4 0])
    (fact "`L` will move to count line from bottom of file when file is shorter"
      (cursor :editing "One\nTwo\nThree" :after "2L") => [1 0])
    (fact "`L` will not move above top of viewport"
      (cursor :editing ten-lines :after "G8L") => [0 0]
      (terminal :editing ten-lines :after "G8L")
        => ["Five"
            "Six"
            "Seven"
            "Eight"
            "Nine"
            "Ten"
            "test.txt" :black :on :white
            ""])))

  (facts "about `H`"
    (fact "`H` moves to the first line in the buffer viewport"
      (cursor :editing ten-lines :after "GH") => [0 0]
      (terminal :editing ten-lines :after "GH")
        => ["Five"
            "Six"
            "Seven"
            "Eight"
            "Nine"
            "Ten"
            "test.txt" :black :on :white
            ""])
    (fact "`H` moves to the count line in the buffer viewport"
      (cursor :editing ten-lines :after "G3H") => [2 0])
    (fact "`H` will not move below the bottom of the buffer viewport"
      (cursor :editing ten-lines :after "10H") => [5 0])

  (facts "about `M`"
    (fact "`M` moves to the middle line of the viewport when buffer has more lines than the buffer viewport"
      (cursor :editing ten-lines :after "M") => [2 0]
      (terminal :editing ten-lines :after "M")
        => ["One"
            "Two"
            "Three"
            "Four"
            "Five"
            "Six"
            "test.txt" :black :on :white
            ""])
    (fact "`M` moves to the middle line of buffer text when buffer contains fewer lines than the buffer viewport"
      (cursor :editing "One\nTwo\nThree" :after "M") => [1 0])))

(facts "about `gg`"
  (fact "`gg` moves to the first non-blank character on the first line"
    (cursor :editing " ...\n...\nThree" :after "Gllgg") => [0 1])
  (fact "`gg` moves to the firts non-blank character on the counth line"
    (cursor :editing "...\n ...\nThree" :after "ll2gg") => [1 1])
  (fact "`gg` won't move past end-of-file"
    (cursor :editing ten-lines :after "99gg") => [5 0]
    (terminal :editing ten-lines :after "99gg")
      => ["Five"
          "Six"
          "Seven"
          "Eight"
          "Nine"
          "Ten"
          "test.txt" :black :on :white
          ""]))

(facts "about `x`"
  (fact "`x` deletes the current character"
    (terminal :line 0 :editing "One\nTwo\nThree..." :after "x") => "ne")
  (fact "`x` does not fail on zero-character line"
    (terminal :editing "a\nb\nc" :after "xx")
      => [""
          "b"
          "c"
          "~" :blue
          "~" :blue
          "~" :blue
          "test.txt" :black :on :white
          ""])
  (fact "`x` at end of line moves the cursor back"
    (cursor :editing "ab" :after "$x") => [0 0]))

(facts "about `dd`"
  (fact "`dd` deletes the current line"
    (terminal :editing "One\nTwo\nThree..." :after "jdd")
      => ["One"
          "Three..."
          "~" :blue
          "~" :blue
          "~" :blue
          "~" :blue
          "test.txt" :black :on :white
          ""])
  (fact "`dd` moves the cursor to the first non-space"
    (cursor :editing "One\nTwo\n  Three..." :after "jdd") => [1 2])
  (fact "`dd` moves the cursor up when deleting the last line"
    (cursor :editing "One\nTwo\nThree" :after "Gdd") => [1 0])
  (fact "`dd` can delete the only line in a file"
    (terminal :editing "One" :after "dd")
      => [""
          "~" :blue
          "~" :blue
          "~" :blue
          "~" :blue
          "~" :blue
          "test.txt" :black :on :white
          ""]))

(facts "about `%`"
  (fact "`%` moves to a matching closing bracket"
    (cursor :editing "()" :after "%") => [0 1]
    (cursor :editing "(())" :after "%") => [0 3]
    (cursor :editing "[x]" :after "%") => [0 2]
    (cursor :editing "{[)]}]" :after "%") => [0 4])
  (fact "`%` moves to a matching opening bracket"
    (cursor :editing "()" :after "l%") => [0 0]  
    (cursor :editing "(())" :after "$%") => [0 0]
    (cursor :editing "([])" :after "ll%") => [0 1])
  (fact "`%` beeps when no matching paren"
    (editor :editing "((" :after "%") => beeped)
  (fact "`%` beeps when not on a bracket"
    (editor :editing "x" :after "%") => beeped)
  (fact "`%` works across lines"
    (cursor :editing "\n(fact \"x\"\n  (foo :bar) => 42)" :after "G$%") => [1 0]
    (cursor :editing "(fact\n\n  (foo :bar) => 42)" :after "%") => [2 18]))

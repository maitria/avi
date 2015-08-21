(ns avi.normal-mode-test
  (:require [midje.sweet :refer :all] 
            [avi.test-helpers :refer :all]))

(facts "regarding repeating commands"
  (fact "`1` through `9` can be used as repeat counts."
    (editor :editing "0123456789x" :after "1l") => (cursor [0 1])
    (editor :editing "0123456789x" :after "2l") => (cursor [0 2])
    (editor :editing "0123456789x" :after "3l") => (cursor [0 3])
    (editor :editing "0123456789x" :after "4l") => (cursor [0 4])
    (editor :editing "0123456789x" :after "5l") => (cursor [0 5])
    (editor :editing "0123456789x" :after "6l") => (cursor [0 6])
    (editor :editing "0123456789x" :after "7l") => (cursor [0 7])
    (editor :editing "0123456789x" :after "8l") => (cursor [0 8])
    (editor :editing "0123456789x" :after "9l") => (cursor [0 9]))
  (fact "Multiple digits can be used as the repeat count."
    (editor :editing "0000000000111111111" :after "17l") => (cursor [0 17]))
  (fact "`0` can be used in a repeat count."
    (editor :editing "0000000000111111111" :after "10l") => (cursor [0 10]))
  (fact "The repeat goes away after a command is executed."
    (editor :editing "0123456789x" :after "4ll") => (cursor [0 5]))
  (fact "None of the digits clear the repeat count."
    (:count (editor :after "1234567890")) => 1234567890))

(facts "regarding cursor movement"
  (fact "The cursor starts on line 1, column 0."
    (editor) => (cursor [0 0]))

  (facts "about `j`"
    (fact "`j` moves the cursor down one line."
      (editor :after "j") => (cursor [1 0])
      (editor :after "jj") => (cursor [2 0]))
    (fact "`j` can move to a zero-length line."
      (editor :editing "One\n\nTwo" :after "j") => (cursor [1 0]))
    (fact "`j` won't move the cursor below the last line."
      (editor :after "jjjj") => (cursor [3 0])
      (editor :after "jjjj") => beeped)
    (fact "`j` won't place the cursor after the end of the line."
      (editor :editing "Hello\nOne" :after "llllj") => (cursor [1 2]))
    (fact "`j` remembers the last explicitly-set column."
      (editor :editing "Hello\n.\nThere" :after "lljj") => (cursor [2 2])))

  (facts "about `k`"
    (fact "`k` moves the cursor up one line."
      (editor :after "jk") => (cursor [0 0])
      (editor :after "jjjkk") => (cursor [1 0]))
    (fact "`k` won't move above the first line."
      (editor :after "k") => (cursor [0 0])
      (editor :after "k") => beeped
      (editor :after "kj") => did-not-beep)
    (fact "`k` can move to a zero-length line."
      (editor :editing "\nOne" :after "jk") => (cursor [0 0]))
    (fact "`k` won't place the cursor after the end of the line."
      (editor :editing "One\nHello" :after "jllllk") => (cursor [0 2])))

  (facts "about `l`"
    (fact "`l` moves to the right one character."
      (editor :after "l") => (cursor [0 1])
      (editor :after "ll") => (cursor [0 2]))
    (fact "`l` won't move beyond the end of the line."
      (editor :after "lll") => (cursor [0 2])
      (editor :after "lll") => beeped))

  (facts "about `h`"
    (fact "`h` moves to the left one character."
      (editor :after "llh") => (cursor [0 1]))
    (fact "`h` won't move before the beginning of the line."
      (editor :after "h") => (cursor [0 0])
      (editor :after "h") => beeped))

  (facts "about moving to the beginning or end of line"
    (fact "`0` moves to the first character on the line."
      (editor :after "ll0") => (cursor [0 0])
      (editor :editing "\n" :after "0") => did-not-beep)

    (fact "`$` moves to the last character on the line."
      (editor :after "$") => (cursor [0 2]))
    (fact "`^` moves to the first non-space character"
      (editor :editing "bob" :after "$^") => (cursor [0 0])
      (editor :editing "  bob" :after "$^") => (cursor [0 2])
      (editor :editing ".\n\n." :after "j^") => did-not-beep
      (editor :editing "   " :after "0^") => (cursor [0 2])))

  (facts "about `G`"
    (fact "`G` moves to the first non-blank character on the last line"
      (editor :editing "...\n...\n Three" :after "llG") => (cursor [2 1]))
    (fact "`G` moves to the first non-blank character on the line in the count register."
      (editor :editing "...\n ...\nThree" :after "ll2G") => (cursor [1 1]))))

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
      (editor :editing ten-lines :after "7j") => (cursor [5 0]))
        
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
      (editor :editing ten-lines :after "7jk") => (cursor [4 0]))

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
      (editor :editing ten-lines :after "7j6k") => (cursor [0 0])))
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
      (editor :editing ten-lines :after "<C-E>") => (cursor [0 0]))
    (fact "`^E` doesn't move the cursor when unnecessary"
      (editor :editing ten-lines :after "jj<C-E>") => (cursor [1 0]))
    (fact "`^E` won't put the cursor past end-of-line"
      (editor :editing ten-lines :after "3G$3<C-E>") => (cursor [0 3])))
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
      (editor :editing ten-lines :after "7G<C-Y>") => (cursor [5 0]))
    (fact "`^Y` doesn't move the cursor when unnecessary"
      (editor :editing ten-lines :after "<C-E><C-Y>") => (cursor [1 0]))
    (fact "`^Y` won't put the cursor past end-of-line"
      (editor :editing ten-lines :after "7G$<C-Y>") => (cursor [5 2])))
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
      (editor :editing ten-lines :after "jj<C-D>") => (cursor [2 0]))
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
      (editor :editing ten-lines :after "Gk<C-D>") => (cursor [5 0]))
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
      (editor :editing "One\nTwo" :after "<C-D>") => (cursor [1 0])))

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
      (editor :editing ten-lines :after "Gk<C-U>") => (cursor [4 0]))
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
      (editor :editing ten-lines :after "j<C-U>") => (cursor [0 0]))))

(facts "about navigating within the viewport"
  (facts "about `L`"
    (fact "`L` moves to the last line when buffer has fewer lines than the buffer viewport"
      (editor :editing "One\nTwo\nThree" :after "L") => (cursor [2 0]))
    (fact "`L` moves to the last line on the buffer viewport when the file is longer"
      (editor :editing ten-lines :after "L") => (cursor [5 0])
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
      (editor :editing ten-lines :after "<C-E>2L") => (cursor [4 0]))
    (fact "`L` will move to count line from bottom of file when file is shorter"
      (editor :editing "One\nTwo\nThree" :after "2L") => (cursor [1 0]))
    (fact "`L` will not move above top of viewport"
      (editor :editing ten-lines :after "G8L") => (cursor [0 0])
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
      (editor :editing ten-lines :after "GH") => (cursor [0 0])
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
      (editor :editing ten-lines :after "G3H") => (cursor [2 0]))
    (fact "`H` will not move below the bottom of the buffer viewport"
      (editor :editing ten-lines :after "10H") => (cursor [5 0]))

  (facts "about `M`"
    (fact "`M` moves to the middle line of the viewport when buffer has more lines than the buffer viewport"
      (editor :editing ten-lines :after "M") => (cursor [2 0])
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
      (editor :editing "One\nTwo\nThree" :after "M") => (cursor [1 0]))))

(facts "about `gg`"
  (fact "`gg` moves to the first non-blank character on the first line"
    (editor :editing " ...\n...\nThree" :after "Gllgg") => (cursor [0 1]))
  (fact "`gg` moves to the firts non-blank character on the counth line"
    (editor :editing "...\n ...\nThree" :after "ll2gg") => (cursor [1 1]))
  (fact "`gg` won't move past end-of-file"
    (editor :editing ten-lines :after "99gg") => (cursor [5 0])
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
    (terminal :line 0 :editing "a\nb\nc" :after "xx") => "")
  (fact "`x` can be repeated"
    (terminal :line 0 :editing "abcdef" :after "3x") => "def")
  (fact "`x` at end of line moves the cursor back"
    (editor :editing "ab" :after "$x") => (cursor [0 0])))

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
    (editor :editing "One\nTwo\n  Three..." :after "jdd") => (cursor [1 2]))
  (fact "`dd` moves the cursor up when deleting the last line"
    (editor :editing "One\nTwo\nThree" :after "Gdd") => (cursor [1 0]))
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

(facts "about `J`"
  (fact "`J` joins lines"
    (terminal :editing "One\nTwo\nThree..." :after "J")
      => ["One Two"
          "Three..."
          "~" :blue
          "~" :blue
          "~" :blue
          "~" :blue
          "test.txt" :black :on :white
          ""])
  (fact "`3J` joins three lines"
    (terminal :editing "One\nTwo\nThree\nFour\nFive" :after "3J")
      => ["One Two Three Four"
          "Five"
          "~" :blue
          "~" :blue
          "~" :blue
          "~" :blue
          "test.txt" :black :on :white
          ""])
  (fact "`3Ju` leaves file in original state"
    (terminal :editing "One\nTwo\nThree\nFour\nFive" :after "3Ju")
      => ["One"
          "Two"
          "Three"
          "Four"
          "Five"
          "~" :blue
          "test.txt" :black :on :white
          ""]))

(facts "about `%`"
  (fact "`%` moves to a matching closing bracket"
    (editor :editing "()" :after "%") => (cursor [0 1])
    (editor :editing "(())" :after "%") => (cursor [0 3])
    (editor :editing "[x]" :after "%") => (cursor [0 2])
    (editor :editing "{[)]}]" :after "%") => (cursor [0 4]))
  (fact "`%` moves to a matching opening bracket"
    (editor :editing "()" :after "l%") => (cursor [0 0])  
    (editor :editing "(())" :after "$%") => (cursor [0 0])
    (editor :editing "([])" :after "ll%") => (cursor [0 1]))
  (fact "`%` beeps when no matching paren"
    (editor :editing "((" :after "%") => beeped)
  (fact "`%` beeps when not on a bracket"
    (editor :editing "x" :after "%") => beeped)
  (fact "`%` works across lines"
    (editor :editing "\n(fact \"x\"\n  (foo :bar) => 42)" :after "G$%") => (cursor [1 0])
    (editor :editing "(fact\n\n  (foo :bar) => 42)" :after "%") => (cursor [2 18])))

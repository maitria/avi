(ns avi.t-normal-mode
  (:require [midje.sweet :refer :all] 
            [avi.spec-runner :refer :all]
            [avi.test-helpers :refer :all]))

(facts "regarding repeating commands"
  (fact "`1` through `9` can be used as repeat counts."
    (editor :editing "0123456789x" :after "1l") => (point [0 1])
    (editor :editing "0123456789x" :after "2l") => (point [0 2])
    (editor :editing "0123456789x" :after "3l") => (point [0 3])
    (editor :editing "0123456789x" :after "4l") => (point [0 4])
    (editor :editing "0123456789x" :after "5l") => (point [0 5])
    (editor :editing "0123456789x" :after "6l") => (point [0 6])
    (editor :editing "0123456789x" :after "7l") => (point [0 7])
    (editor :editing "0123456789x" :after "8l") => (point [0 8])
    (editor :editing "0123456789x" :after "9l") => (point [0 9]))
  (fact "Multiple digits can be used as the repeat count."
    (editor :editing "0000000000111111111" :after "17l") => (point [0 17]))
  (fact "`0` can be used in a repeat count."
    (editor :editing "0000000000111111111" :after "10l") => (point [0 10]))
  (fact "The repeat goes away after a command is executed."
    (editor :editing "0123456789x" :after "4ll") => (point [0 5])))

(facts "regarding point movement"
  (fact "The point starts on line 1, column 0."
    (editor) => (point [0 0]))

  (facts "about `j`"
    (fact "`j` moves the point down one line."
      (editor :after "j") => (point [1 0])
      (editor :after "jj") => (point [2 0]))
    (fact "`j` can move to a zero-length line."
      (editor :editing "One\n\nTwo" :after "j") => (point [1 0]))
    (fact "`j` won't move the point below the last line."
      (editor :after "jjjj") => (point [3 0])
      (editor :after "jjjj") => beeped)
    (fact "`j` won't place the point after the end of the line."
      (editor :editing "Hello\nOne" :after "llllj") => (point [1 2]))
    (fact "`j` remembers the last explicitly-set column."
      (editor :editing "Hello\n.\nThere" :after "lljj") => (point [2 2])))

  (facts "about `k`"
    (fact "`k` moves the point up one line."
      (editor :after "jk") => (point [0 0])
      (editor :after "jjjkk") => (point [1 0]))
    (fact "`k` won't move above the first line."
      (editor :after "k") => (point [0 0])
      (editor :after "k") => beeped
      (editor :after "kj") => did-not-beep)
    (fact "`k` can move to a zero-length line."
      (editor :editing "\nOne" :after "jk") => (point [0 0]))
    (fact "`k` won't place the point after the end of the line."
      (editor :editing "One\nHello" :after "jllllk") => (point [0 2])))

  (facts "about `l`"
    (fact "`l` moves to the right one character."
      (editor :after "l") => (point [0 1])
      (editor :after "ll") => (point [0 2]))
    (fact "`l` won't move beyond the end of the line."
      (editor :editing "One\n.." :after "lll") => (point [0 2])
      (editor :editing "One\n.." :after "lll") => beeped)
      (editor :editing "One\n.." :after "9l") => (point [0 2])
      (editor :editing "One\n.." :after "9l") =not=> beeped)

  (facts "about `h`"
    (fact "`h` moves to the left one character."
      (editor :after "llh") => (point [0 1]))
    (fact "`h` won't move before the beginning of the line."
      (editor :after "h") => (point [0 0])
      (editor :after "h") => beeped)
    (fact "`{count}h` moves as far left as possible"
      (editor :editing "abcdefgh" :after "$99h") => (point [0 0])))

  (facts "about moving to the beginning or end of line"
    (fact "`0` moves to the first character on the line."
      (editor :after "ll0") => (point [0 0])
      (editor :editing "\n" :after "0") => did-not-beep)

    (fact "`$` moves to the last character on the line"
      (editor :after "$") => (point [0 2]))
    (fact "`$jj` stays on last character of the line"
      (editor :editing "Hi\nHello\nWha" :after "$jj") => (point [2 2]))
    (fact "`^` moves to the first non-space character"
      (editor :editing "bob" :after "$^") => (point [0 0])
      (editor :editing "  bob" :after "$^") => (point [0 2])
      (editor :editing ".\n\n." :after "j^") => did-not-beep
      (editor :editing "   " :after "0^") => (point [0 2])))

  (facts "about `G`"
    (fact "`G` moves to the first non-blank character on the last line"
      (editor :editing "...\n...\n Three" :after "llG") => (point [2 1]))
    (fact "`G` moves to the first non-blank character on the line in the count register."
      (editor :editing "...\n ...\nThree" :after "ll2G") => (point [1 1]))))

(facts "regarding scrolling"
  (fact "line-wise point movement will keep the point in the viewport"
    (fact "can scroll down some lines"
      (editor :editing ten-lines :after "7j")
        => (terminal ["Three"
                      "Four"
                      "Five"
                      "Six"
                      "Seven"
                      "Eight"
                      "test.txt" :black :on :white
                      ""])
      (editor :editing ten-lines :after "7j") => (point [5 0]))
        
    (fact "viewport stays when moving back up"
      (editor :editing ten-lines :after "7jk")
        => (terminal ["Three"
                      "Four"
                      "Five"
                      "Six"
                      "Seven"
                      "Eight"
                      "test.txt" :black :on :white
                      ""])
      (editor :editing ten-lines :after "7jk") => (point [4 0]))

    (fact "can scroll up some lines"
      (editor :editing ten-lines :after "7j6k")
        => (terminal ["Two"
                      "Three"
                      "Four"
                      "Five"
                      "Six"
                      "Seven"
                      "test.txt" :black :on :white
                      ""])
      (editor :editing ten-lines :after "7j6k") => (point [0 0])))
  (facts "about `^E`"
    (fact "`^E` scrolls the buffer down one line"
      (editor :editing ten-lines :after "<C-E>")
        => (terminal ["Two"
                      "Three"
                      "Four"
                      "Five"
                      "Six"
                      "Seven"
                      "test.txt" :black :on :white
                      ""]))
    (fact "`^E` moves the point down to keep it in the viewport"
      (editor :editing ten-lines :after "<C-E>") => (point [0 0]))
    (fact "`^E` doesn't move the point when unnecessary"
      (editor :editing ten-lines :after "jj<C-E>") => (point [1 0]))
    (fact "`^E` won't put the point past end-of-line"
      (editor :editing ten-lines :after "3G$3<C-E>") => (point [0 3])))
  (facts "about `^Y`"
    (fact "`^Y` scrolls the buffer up one line"
      (editor :editing ten-lines :after "<C-E><C-Y>")
        => (terminal ["One"
                      "Two"
                      "Three"
                      "Four"
                      "Five"
                      "Six"
                      "test.txt" :black :on :white
                      ""]))
    (fact "`^Y` moves the point up to keep it in the viewport"
      (editor :editing ten-lines :after "7G<C-Y>") => (point [5 0]))
    (fact "`^Y` doesn't move the point when unnecessary"
      (editor :editing ten-lines :after "<C-E><C-Y>") => (point [1 0]))
    (fact "`^Y` won't put the point past end-of-line"
      (editor :editing ten-lines :after "7G$<C-Y>") => (point [5 2])))
  (facts "about `^D`"
    (fact "`^D` scrolls down half a page"
      (editor :editing ten-lines :after "<C-D>")
        => (terminal ["Four"
                      "Five"
                      "Six"
                      "Seven"
                      "Eight"
                      "Nine"
                      "test.txt" :black :on :white
                      ""]))
    (fact "`^D` moves the point down half a page"
      (editor :editing ten-lines :after "jj<C-D>") => (point [2 0]))
    (fact "`^D` won't scroll past end-of-file"
      (editor :editing ten-lines :after "<C-D><C-D><C-D>")
        => (terminal ["Five"
                      "Six"
                      "Seven"
                      "Eight"
                      "Nine"
                      "Ten"
                      "test.txt" :black :on :white
                      ""]))
    (fact "`^D` near end-of-file moves the point to last line (and not past)"
      (editor :editing ten-lines :after "Gk<C-D>") => (point [5 0]))
    (fact "`^D` on last line beeps"
      (editor :editing ten-lines :after "G<C-D>") => beeped)
    (fact "`^D` won't scroll when file is shorter than buffer viewport"
      (editor :after "<C-D>")
        => (terminal ["One"
                      "Two"
                      "Three"
                      "."
                      "~" :blue
                      "~" :blue
                      "test.txt" :black :on :white
                      ""]))
    (fact "`^D` won't move point past end-of-file when file is shorter than buffer viewport"
      (editor :editing "One\nTwo" :after "<C-D>") => (point [1 0])))

  (facts "about `^U`"
    (fact "`^U` on first line beeps"
      (editor :after "<C-U>") => beeped)
    (fact "`^U` scrolls up a half page"
      (editor :editing ten-lines :after "<C-D><C-U>")
        => (terminal ["One"
                      "Two"
                      "Three"
                      "Four"
                      "Five"
                      "Six"
                      "test.txt" :black :on :white
                      ""]))
    (fact "`^U` moves the point up half a page"
      (editor :editing ten-lines :after "Gk<C-U>") => (point [4 0]))
    (fact "`^U` does not scroll to before first line of file"
      (editor :editing ten-lines :after "<C-E><C-U>")
        => (terminal ["One"
                      "Two"
                      "Three"
                      "Four"
                      "Five"
                      "Six"
                      "test.txt" :black :on :white
                      ""]))
    (fact "`^U` does not move point before beginning of file"
      (editor :editing ten-lines :after "j<C-U>") => (point [0 0]))))

(facts "about navigating within the viewport"
  (facts "about `L`"
    (fact "`L` moves to the last line when buffer has fewer lines than the buffer viewport"
      (editor :editing "One\nTwo\nThree" :after "L") => (point [2 0]))
    (fact "`L` moves to the last line on the buffer viewport when the file is longer"
      (editor :editing ten-lines-indented :after "L") => (point [5 2])
      (editor :editing ten-lines :after "L")
        => (terminal ["One"
                      "Two"
                      "Three"
                      "Four"
                      "Five"
                      "Six"
                      "test.txt" :black :on :white
                      ""]))
    (fact "`L` will move to count line from bottom of viewport"
      (editor :editing ten-lines-indented :after "<C-E>2L") => (point [4 2]))
    (fact "`L` will move to count line from bottom of file when file is shorter"
      (editor :editing "One\nTwo\nThree" :after "2L") => (point [1 0]))
    (fact "`L` will not move above top of viewport"
      (editor :editing ten-lines :after "G8L") => (point [0 0])
      (editor :editing ten-lines :after "G8L")
        => (terminal ["Five"
                      "Six"
                      "Seven"
                      "Eight"
                      "Nine"
                      "Ten"
                      "test.txt" :black :on :white
                      ""]))))

  (facts "about `H`"
    (fact "`H` moves to the first line in the buffer viewport"
      (editor :editing ten-lines-indented :after "G$H") => (point [0 2])
      (editor :editing ten-lines :after "GH")
        => (terminal ["Five"
                      "Six"
                      "Seven"
                      "Eight"
                      "Nine"
                      "Ten"
                      "test.txt" :black :on :white
                      ""]))
    (fact "`H` moves to the count line in the buffer viewport"
      (editor :editing ten-lines :after "G3H") => (point [2 0]))
    (fact "`H` will not move below the bottom of the buffer viewport"
      (editor :editing ten-lines :after "11H") => (point [5 0]))

  (facts "about `M`"
    (fact "`M` moves to the middle line of the viewport when buffer has more lines than the buffer viewport"
      (editor :editing ten-lines-indented :after "M") => (point [2 2])
      (editor :editing ten-lines :after "M")
        => (terminal ["One"
                      "Two"
                      "Three"
                      "Four"
                      "Five"
                      "Six"
                      "test.txt" :black :on :white
                      ""]))
    (fact "`M` moves to the middle line of buffer text when buffer contains fewer lines than the buffer viewport"
      (editor :editing "One\nTwo\nThree" :after "M") => (point [1 0]))))

(facts "about `gg`"
  (fact "`gg` moves to the first non-blank character on the first line"
    (editor :editing " ...\n...\nThree" :after "Gllgg") => (point [0 1]))
  (fact "`gg` moves to the firts non-blank character on the counth line"
    (editor :editing "...\n ...\nThree" :after "ll2gg") => (point [1 1]))
  (fact "`gg` won't move past end-of-file"
    (editor :editing ten-lines :after "99gg") => (point [5 0])
    (editor :editing ten-lines :after "99gg")
      => (terminal ["Five"
                    "Six"
                    "Seven"
                    "Eight"
                    "Nine"
                    "Ten"
                    "test.txt" :black :on :white
                    ""])))

(facts "about `x`"
  (fact "`x` deletes the current character"
    (editor :editing "One\nTwo\nThree..." :after "x") => (line 0 "ne"))
  (fact "`x` does not fail on zero-character line"
    (editor :editing "a\nb\nc" :after "xx") => (line 0 ""))
  (fact "`x` can be repeated"
    (editor :editing "abcdef" :after "3x") => (line 0 "def"))
  (fact "`x` at end of line moves the point back"
    (editor :editing "ab" :after "$x") => (point [0 0])))

(facts "about `dd`"
  (fact "`dd` deletes the current line"
    (editor :editing "One\nTwo\nThree..." :after "jdd")
      => (terminal ["One"
                    "Three..."
                    "~" :blue
                    "~" :blue
                    "~" :blue
                    "~" :blue
                    "test.txt" :black :on :white
                    ""]))
  (fact "`dd` moves the point to the first non-space"
    (editor :editing "One\nTwo\n  Three..." :after "jdd") => (point [1 2]))
  (fact "`dd` moves the point up when deleting the last line"
    (editor :editing "One\nTwo\nThree" :after "Gdd") => (point [1 0]))
  (fact "`dd` can delete the only line in a file"
    (editor :editing "One" :after "dd")
      => (terminal [""
                    "~" :blue
                    "~" :blue
                    "~" :blue
                    "~" :blue
                    "~" :blue
                    "test.txt" :black :on :white
                    ""])))

(tabular
  (facts "about `J`"
    (let [result (editor :editing "One\nTwo\nThree\nFour" :after ?keys)]
      result => (contents ?after)
      result => (point ?point)))
  ?keys ?point   ?after
  "J"   [0 3]  "One Two\nThree\nFour"
  "3J"  [0 7]  "One Two Three\nFour"
  "4J"  [0 13] "One Two Three Four"
  "3Ju" [0 0]  "One\nTwo\nThree\nFour")

(facts "about `%`"
  (fact "`%` moves to a matching closing bracket"
    (editor :editing "()" :after "%") => (point [0 1])
    (editor :editing "(())" :after "%") => (point [0 3])
    (editor :editing "[x]" :after "%") => (point [0 2])
    (editor :editing "{[)]}]" :after "%") => (point [0 4]))
  (fact "`%` moves to a matching opening bracket"
    (editor :editing "()" :after "l%") => (point [0 0])  
    (editor :editing "(())" :after "$%") => (point [0 0])
    (editor :editing "([])" :after "ll%") => (point [0 1]))
  (fact "`%` beeps when no matching paren"
    (editor :editing "((" :after "%") => beeped)
  (fact "`%` beeps when not on a bracket"
    (editor :editing "x" :after "%") => beeped)
  (fact "`%` works across lines"
    (editor :editing "\n(fact \"x\"\n  (foo :bar) => 42)" :after "G$%") => (point [1 0])
    (editor :editing "(fact\n\n  (foo :bar) => 42)" :after "%") => (point [2 18])))

(facts "about `f`"
  (fact "`fx` moves to the next `x`"
    (editor :editing "helloxthere" :after "fx") => (point [0 5])
    (editor :editing "...x...x" :after "fx") => (point [0 3])
    (editor :editing "...x...x" :after "3lfx") => (point [0 7]))
  (fact "`fx` beeps if there's no `x`"
    (editor :editing "..." :after "fx") => beeped)
  (fact "`fy` moves to the next `y`"
    (editor :editing "helloythere" :after "fy") => (point [0 5]))
  (fact "`f0` moves to the next `0`"
    (editor :editing "hell0t" :after "f0") => (point [0 4]))
  (fact "`f1` moves to the next `1`"
    (editor :editing "he1lo" :after "f1") => (point [0 2]))
  (fact "`{count}fx` works"
    (editor :editing "abx cdx efx " :after "3fx") => (point [0 10])))

(facts "about `F`"
  (fact "`Fx` moves to the previous `x`"
    (editor :editing "helloxthere" :after "$Fx") => (point [0 5])
    (editor :editing "helloxtherex" :after "$Fx") => (point [0 5]))
  (fact "`Fx` beeps if there is no previous `x`"
    (editor :editing "helloxthere" :after "3lFx") => beeped))

(facts "about `T`"
  (fact "`Tx` moves to just after the previous `x`"
    (editor :editing "helloxthere" :after "$Tx") => (point [0 6])
    (editor :editing "helloxtherex" :after "$Tx") => (point [0 6]))
  (fact "`Tx` beeps if there is no previous `x`"
    (editor :editing "helloxthere" :after "3lTx") => beeped))

(facts "about `t`"
  (fact "`tx` moves to just before the next `x`"
    (editor :editing "helloxthere" :after "tx") => (point [0 4]))
  (fact "`tx` beeps if there's no `x`"
    (editor :editing "..." :after "tx") => beeped))

(tabular
  (facts "about `d<Motion>` which work"
    (let [result #(editor :editing ?before :after ?keys)]
      (result) => (contents ?after)
      (result) => (point ?point)
      (result) =not=> beeped))

  ?before             ?keys     ?point  ?after
  "abcd)f"            "lldf)"   [0 2] "abf"
  "abcd/f"            "lldf/"   [0 2] "abf"
  "abcdef"            "lldl"    [0 2] "abdef"
  "abcdef"            "ll9dl"   [0 1] "ab"
  "abcdef"            "lldh"    [0 1] "acdef"
  "abcdef"            "$dl"     [0 4] "abcde"
  "a\nb\nc"           "lldj"    [0 0] "c"
  "a\nb"              "dj"      [0 0] ""
  "a\nb\nc"           "jdj"     [0 0] "a"
  "1234"              "lld$"    [0 1] "12"
  "1234"              "lld0"    [0 0] "34"
  "  abcdefgh"        "5ld^"    [0 2] "  defgh"
  "a\nb\nc\nd"        "jjdgg"   [0 0] "d"
  "a\nb\nc\nd"        "jdgg"    [0 0] "c\nd"
  "a\nb\nc\nd"        "jjdH"    [0 0] "d"
  "a\nb\nc\nd"        "jdH"     [0 0] "c\nd"
  ten-lines           "jjdL"    [2 0] #"^One\nTwo\nSeven\n"
  ten-lines           "jdM"     [1 0] #"^One\nFour\n"
  "a\nb\nc\nd"        "Gkdk"    [1 0] "a\nd"
  "aa\nbb\ncc\ndd"    "lj3dG"   [1 0] "aa\ndd"
  "a\nb\nc\nd"        "dG"      [0 0] ""
  "a\nb\nc\nd"        "jdk"     [0 0] "c\nd"
  "a\nb\nc\nd"        "3Gdku"   [2 0] "a\nb\nc\nd"
  "abcdefghi"         "$99dh"   [0 0] "i"
  "a\nb\nc\nd"        "2j3dj"   [1 0] "a\nb"
  "a\nb\nc\nd"        "2G7dk"   [0 0] "c\nd")

(tabular
  (facts "about `d<Motion>` which fail"
    (let [result (editor :editing ?editings :after ?keys)]
      (fact "failed delete motions beep"
        result => beeped)
      (fact "failed delete motions do not change file contents"
        result => (contents ?editings))))
  ?editings ?keys
  "y12x"    "ldf)"
  "a\nb\nc" "Gdj"
  "a\nb\nc" "dk")

(facts "about `D`"
  (fact "`D` deletes to the end-of-line"
    (editor :editing "1234" :after "llD") => (line 0 "12")))

(facts-about "`w` moves to the beginning of the next word"
  editing        after   point
  "hello world"  "w"     [0 6]
  "      world"  "w"     [0 6]
  "ab cd ef gh"  "3w"    [0 9]
  "ab cd\nef gh" "3w"    [1 3]
  "ab* cd"       "w"     [0 2]
  "ab *cd"       "w"     [0 3]
  "*)ab"         "w"     [0 2]
  "*)  ab"       "w"     [0 4])

(facts "about `w`"
  (tabular
    (fact "`w` stops on zero-length lines"
      (editor :editing ?editing :after ?after) => (point ?point))
    ?editing       ?after  ?point
    "ab\n\ncd"     "w"     [1 0]
    "  \n\ncd"     "w"     [1 0]
    "))\n\ncd"     "w"     [1 0])
  (fact "`w` will move to the end of file"
    (editor :editing "hello" :after "w") => (point [0 4])
    (editor :editing "h" :after "w") => beeped)
  (fact "`{count}w` stops at end-of-file"
    (editor :editing "hello world" :after "5w") => (point [0 10]))
  (fact "`dw` deletes to end of current word"
    (editor :editing "hello world" :after "ldw") => (line 0 "hworld")
    (editor :editing "hello world" :after "ldw") => (point [0 1]))
  (fact "`dw` obeys {count}"
     (editor :editing "hello world there" :after "l2dw") => (line 0 "hthere")
     (editor :editing "hello world there" :after "l2dw") => (point [0 1]))
  (fact "`dw` doesn't delete past end-of-line"
    (editor :editing "hello\n  world" :after "ldw") => (contents "h\n  world"))
  (fact "`dw` deletes to end-of-file"
    (editor :editing "hello" :after "ldw") => (contents "h")))

(tabular
  (facts "about `W`"
    (editor :editing ?editing :after ?after) => (point ?point))
  ?editing       ?after ?point
  "hello world"  "W"    [0 6]
  "hel)) wordl"  "W"    [0 6]
  "      eorld"  "W"    [0 6]
  "he??o\n\nwha" "W"    [1 0])

(facts-about "`ge` and `gE`"
  editing       after  point
  "hello  "     "$ge"  [0 4]
  "hello world" "$ge"  [0 4]
  "hello world" "$5ge" [0 0]
  "hel//  "     "$gE"  [0 4]
  "(hel/ ??rld" "$gE"  [0 4]
  "(hel/ ??rld" "$5gE" [0 0]
  "hello\n\nfo" "Gge"  [1 0]
  "hello\n\nfo" "GgE"  [1 0])

(facts-about "`e`"
  editing         after  point
  "hello world"   "e"    [0 4]
  "hello world"   "2e"   [0 10]
  "  hello wor"   "e"    [0 6]
  "  hello wor"   "ee"   [0 10]
  "))//? world"   "e"    [0 4]
  "))//? !!<>?"   "2e"   [0 10]
  "  ))//? wor"   "e"    [0 6]
  "    "          "e"    [0 3]
  "    "          "2e"   [0 3]
  "foo"           "e"    [0 2]
  "???"           "e"    [0 2]
  "hello\nworld"  "e"    [0 4]
  "  \n\nwat"     "e"    [2 2] ; `e` does not stop on zero-length lines
  "  \n hi"       "e"    [1 2]
  "hello??"       "e"    [0 4]
  ".?//.wo"       "e"    [0 4])

(facts-about "`E`"
  editing         after  point
  "he//o world"   "E"    [0 4]
  "  he//o wor"   "E"    [0 6]
  "  \n\nw?t"     "E"    [2 2]
  "  \n hi"       "E"    [1 2])

(facts-about "normal-mode `b`"
  editing          after  point
  "hello world"    "$b"   [0 6]
  "a hello world"  "$bb"  [0 2]
  "a hello world"  "$2b"  [0 2]
  "a hello\nworld" "G$b"  [1 0]
  "a hello\nworld" "G$2b" [0 2]
  "a\n\nwhat"      "Gb"   [1 0])

(tabular
  (facts "about `iw`"
    (editor :editing ?editing :after ?after) => (point ?point)
    (editor :editing ?editing :after ?after) => (contents ?editing-after))

  ?editing        ?after  ?point  ?editing-after
  "hello world"   "diw"   [0 0] " world"
  "hello world"   "ldiw"  [0 0] " world"
  "hello world"   "wdiw"  [0 5] "hello ")

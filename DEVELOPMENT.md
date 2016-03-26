## Glossary

There is a **terminal window**, owned by the OS. It has a minimize and a close button, for example.

Avi has an **editor viewport**, and this should take up the whole terminal window. (Someday it'll have **tabs** and **panes**.)

A **document** is the analog of a **buffer** in Vim; it contains the name and contents of a document.

A **pane** is a rectangular area of the screen.
A **lens** connects a pane to a document (the lens tracks the cursor, the offset into the document, and other kinds of presentation concerns).
So every pane has a lens, but a document can have more than one lens.

Also inside the editor viewport, below any panes, is a **status line**. That's where you probably see the filename you're editing in reverse text, when you're working in Vim.

Also inside the editor viewport, below the status line, is the **command line**. That's where things like `:q` or `:sp README.md` will appear.

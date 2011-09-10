;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sql-mode.el - modes for editing SQL code and interacting with SQL servers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Author:        Peter D. Pezaris <pez@atlantic2.sbi.com>
;;  Maintainer:    sql-mode-help@atlantic2.sbi.com
;;  Version:       0.922 (beta)
;;  Last Modified: Fri Jan 19 10:40:21 1996
;;  Keywords:      sql editing major-mode languages
;;
;;  Copyright © 1994, 1995, 1996 Peter D. Pezaris
;;
;;  This file is not part of GNU Emacs
;;
;;  This program is free software; you can redistribute it and/or
;;  modify it under the terms of the GNU General Public License as
;;  published by the Free Software Foundation; either version 1, or (at
;;  your option) any later version.
;;
;;  This program is distributed in the hope that it will be useful, but
;;  WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;  General Public License for more details.
;;
;;  You should have received a copy of the GNU General Public License
;;  along with this program; if not, write to the Free Software
;;  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Description:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  sql-mode.el offers four major modes relating to SQL code:
;;
;;  sql-mode              major mode for editing SQL code
;;  sql-batch-mode        major mode for entering and execution of SQL
;;                        commands in batches
;;  sql-interactive-mode  major mode for interaction with SQL servers
;;  sql-results-mode      major mode for viewing results returned by the
;;                        execution of commands in sql-batch-mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Features:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  New features to this release are marked with a '+'
;;
;;  o Batch editing of sql commands
;;  o Interactive editing of sql commands
;;  o Warnings for missing `where' clauses
;;  o Warnings for number of rows affected with updates, deletes, etc.
;;  o Command history in batch and interactive modes
;;  o Abbrev definitions for sql commands
;;  o Specialized syntax table for sql code
;;  o Extensive comment and un-comment functionality
;;  o Server/user/password/database associations using mnemonics or menus
;;  o Menubar support
;;  o Popup menu support
;;  o Drag scrolling of results buffers
;;  o Full font-lock support
;;  o Automatic indentation of sql code (still very alpha)
;;  o Printing of results buffers with support for enscript
;;  o Minibuffer completion on servers and users
;;  o Keyword completion
;;  o Table name completion
;;  o Column name completion
;;  o Stored procedure name completion
;;  o Database name completion
;;  o User name completion
;;  + Column value completion
;;  + Operator completion
;;  o Options menu, including save current options
;;  o Changes are marked (highlighted) in results buffers
;;  o Linked batch and results buffers
;;  o Improved table header management
;;  o Global command history
;;  o Top ten list of commonly used queries
;;  o Automagically builds server list from interfaces file
;;  o Error parsing of results buffers
;;  o SQL Mode specific help system
;;  o SQL Mode specific toolbar
;;  o Loading of stored procedures
;;  o Big menubar for novice users
;;  o bcp in and out
;;  o Canned inserts into a table
;;  o Perform updates simply by typing in the results buffer
;;  o Background evaluation so that you can continue to use emacs
;;  + Customizable subprocess behavior
;;  + bcp support for arbitrary rows returned in results buffer
;;  + Dynamic query building
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Installation:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  SQL Mode was written for XEmacs (formerly Lucid Emacs) version 19.8 or
;;  later.  Version 19.10 or later is recommended.  Many features in SQL Mode,
;;  like toolbars and horizontal scrollbars, rely on later versions of XEmacs.
;;
;;  In addition, sql-mode now works with FSF Emacs version 19 in a slightly
;;  limited way.  The package easymenu is required if you are using FSF
;;  Emacs.  SQL Mode has been tested for FSF Emacs version 19.28.  It may
;;  work with some earlier versions, but has not been tested.  Toolbars are
;;  not supported in FSF Emacs version 19.28.
;;
;;  To install SQL Mode, you need to add three lines to your $HOME/.emacs
;;  file.  The first line tells emacs where to look for SQL Mode, the
;;  second line load SQL Mode, and the third line does the initialization.
;;  For things to work properly, you need all three things to happen in
;;  that order.  If you put *all* off the .el files that came with the
;;  distribution in a directory in your load-path, then don't bother with
;;  the first line.
;;
;;      (setq load-path (cons "<PATH-TO-SQL-MODE>/" load-path))
;;      (require 'sql-mode)
;;      (sql-initialize)
;;
;;  To make full use of the help system, you will need to copy the file
;;  SQL-MODE-README into the directory specified by the variable
;;  `data-directory'.  The file should have come with the SQL Mode
;;  distribution.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic Usage:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  INVOCATION
;;
;;  To automatically enter sql-mode when editing a file with a ".sql", a
;;  ".tbl" or a ".sp" extension, add the following to your .emacs file.
;;
;;      (autoload 'sql-mode "sql-mode" "SQL Editing Mode" t)
;;      (setq auto-mode-alist
;;         (append '(("\\.sql$" . sql-mode)
;;                   ("\\.tbl$" . sql-mode)
;;                   ("\\.sp$"  . sql-mode))
;;                 auto-mode-alist))
;;
;;  sql-batch-mode and sql-interactive-mode are invoked with
;;  M-x sql-batch-mode and M-x sql-interactive-mode respectively.  You will
;;  be prompted for the SQL server, the login id of the user, and the
;;  password.  Passwords are echoed with `*' characters as you are typing
;;  them by default.  If you would rather see the password as you are
;;  typing it, see the variable `sql-secure-passwords'.
;;
;;  EVALUATION
;;
;;  In an sql-batch-mode buffer, you can evaluate the buffer by invoking
;;  sql-evaluate-buffer (bound to C-c C-e by default, and also M-i for
;;  backwards compatibility).  A region can be evaluated by passing an
;;  argument to sql-evaluate-buffer.
;;
;;  In an sql-interactive-mode buffer, you can evaluate the command you just
;;  entered by typing `go' followed by RETURN (if you are using isql) or
;;  type a semicolon at the end of the line followed by RETURN (if you are
;;  using fsql).
;;
;;  One very important user-definable variable is `sql-command'.  This
;;  variable should be set to "isql", or "fsql" etc., as appropriate.  See
;;  the section on customization for details on how to set sql-command.
;;  Also make sure that the sql-command is in your path.
;;
;;  Switches can be passed to sql-command by setting the variable
;;  sql-batch-command-switches.  Do not, for instance, set sql-command to
;;  "isql -i file".  Instead, set sql-command to "isql" and 
;;  sql-batch-command-switches to "-i file".
;;
;;  COMMENTING
;;
;;  Lines of code can be commented out and un-commented out using various
;;  sql-comment- functions.  The most basic (and most useful) is
;;  sql-comment-line-toggle.  This function will comment out a line of
;;  code if it is not already commented, or un-comment it if it is
;;  commented.  This function is bound to C-c C-c in sql-mode and
;;  sql-batch-mode buffers by default.
;;
;;  If the region is active, sql-comment-line-toggle will invoke the
;;  function sql-comment-region-toggle, having the effect of toggling the
;;  state of commentedness (yes, I know, that's probably not a word) on
;;  every line in the region.
;;
;;  COMMAND HISTORY
;;
;;  You can scan through your previous commands with sql-previous-history
;;  and sql-next-history (bound to M-p and M-n in sql-batch-mode buffers
;;  by default).  As you evaluate new commands, they are added to the
;;  history list.  The most recent 40 command are kept by default.
;;
;;  DRAG SCROLLING
;;
;;  sql-mode supports drag-scrolling.  By default this is bound to
;;  shift-button1 in sql-interactive-mode and sql-results-mode buffers.  To
;;  scroll the results, simply click and hold the left mouse button while
;;  holding the shift key, and drag the mouse.  The text should scroll
;;  appropriately.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customization:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  To change the value of a user-definable variable, don't change the file
;;  sql-mode.el.  Instead, put a line in either your .emacs file, or the
;;  file $HOME/.sql-mode, as follows:
;;
;;      (setq VARIABLE VALUE)
;;
;;  The usage of the file $HOME/.sql-mode removes some of the clutter from
;;  your .emacs file, speeds up the loading of Emacs, and is therefore
;;  recommended.  The file $HOME/.sql-mode will be loaded once, when
;;  you invoke the function `sql-initialize'.  Subsequent loadings can be
;;  done manually with M-x load-file, or with a direct call to the function
;;  `sql-load-customizations'.
;;
;;  Similarly to changing variable values, if you want to change key
;;  bindings, don't alter the file sql-mode.el, but instead use the
;;  function define-key.  For instance, to bind the key sequence C-c C-a
;;  to the function sql-association-mode, add the following line to your 
;;  $HOME/.emacs file *after* you load sql-mode.el, or add it to your
;;  $HOME/.sql-mode file:
;;
;;      (define-key sql-mode-map "\C-c\C-a" 'sql-association-mode)
;;
;;  This should be used merely as an example, since sql-association-mode
;;  is already bound to C-c C-a by default.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Advanced Usage:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  ASSOCIATION
;;
;;  One of the most convienient features is the ability to specify
;;  server/login-id/password/database associations, and log into a specific
;;  server as a specific user (and optionally use a specific database)
;;  with a single key-stroke, without having to specify a password.  To do
;;  this, you need to specify an sql-asociation-alist.  Here is an example
;;  that features two labels, three separators, and four entries.  The
;;  separators and labels are for decorating the popup-menus.
;;
;;  For each association, there are two required fields, and two optional
;;  ones.  The server and login-id are required, but password and database
;;  are optional.  If no password is supplied, you will be prompted for one
;;  when you invoke the association.  If no database is supplied, the
;;  default (master) database is simply used.
;;
;;  By default the entry in the menubar will contain the mnemonic, the
;;  server, and the user separated by spaces.  If you prefer a different
;;  string, you can include it as the third element in the list.
;;
;;      (setq sql-association-alist
;;  <1>       '(("-" ("LABEL1" "" ""))
;;  <2>         ("-" ("----" "" ""))
;;  <3>         ("MNEMONIC1" ("SERVER1" "LOGIN-ID1" "PASSWORD1" "DATABASE1"))
;;  <4>         ("MNEMONIC2" ("SERVER2" "LOGIN-ID2" "PASSWORD2") "ALIAS2")
;;  <5>         ("-" ("----" "" ""))
;;  <6>         ("-" ("LABEL2" "" ""))
;;  <7>         ("-" ("----" "" ""))
;;  <8>         ("MNEMONIC3" ("SERVER3" "LOGIN-ID3" nil "DATABASE3") "ALIAS3")
;;  <9>         ("MNEMONIC4" ("SERVER4" "LOGIN-ID4" "PASSWORD4"))))
;;
;;  <1> a label
;;  <2> a separator
;;  <3> this association has all four components of an association
;;  <4> this association doesn't specify a database, but has a menubar alias
;;  <5> another separator
;;  <6> another label
;;  <7> yet another separator
;;  <8> this association specifies a database, but no password.  It also
;;      has a menubar alias
;;  <9> this association is the same format as <4> -- the most common
;;
;;  MNEMONIC should be a string of one or more characters that you will use 
;;  to reference your association.  You should have a unique mnemonic for
;;  each association, otherwise you will be unable to reference the
;;  duplicates.
;;
;;  This should be put in your $HOME/.sql-mode file.
;;
;;  WARNING: You may have un-encrypted passwords in this file.  This may
;;           pose a security risk.  At the very least, you should change
;;           the permissions on this file so that only you can read it.
;;           If this is unacceptable, you should omit the password entries.
;;           Omitting the passwords will, however, require you to type
;;           them in on each invocation of either sql-batch-mode or
;;           sql-interactive-mode.
;;
;;  To utilize your association list, invoke sql-association-mode.  You
;;  will be prompted for your association mnemonic, and you will get a
;;  new sql-batch-mode buffer with the appropriate server, user, password,
;;  and database set.  If you prefer using the menubar, there should be
;;  an entry off the top-level menu (under either Apps, Utilities or
;;  SQL-Assoc depending on which version of Emacs you are running) called
;;  "Use Association...".  This menu should contain an entry for each
;;  association you have defined.  By selecting one of them you can easily
;;  switch to a new server/login-id/password/database association.
;;
;;  In addition, you can select an association by invoking
;;  sql-popup-association-menu (bound to button3 in sql-batch-mode and
;;  sql-interactive-mode buffers by default).
;;
;;  If the value of the variable `sql-add-to-menu-bar' is non-nil,
;;  associations will be accessible through the `Apps', `Utilities', or
;;  `SQL-Assoc' menu, or under the menu specified by `sql-parent-menu'.
;;  This is perhaps the best way to utilize your associations, since they
;;  can be launched from any buffer.
;;
;;  RESULTS BUFFERS
;;
;;  Results buffers can be configured to be disposable -- each new result
;;  will overwrite the previous (the default).  They can also be saved, so
;;  that every results buffer is given a unique buffer name.  If you have
;;  disposable results buffers (sql-save-all-results set to nil), and wish
;;  to save a particular buffer, invoke sql-pop-and-rename-buffer.
;;
;;  It is recommended that you keep results buffers in their default
;;  state (disposable), since the results are saved in the history of the
;;  buffer.
;;
;;  Results buffers can also be configured to appear in a separate frame.
;;  Set sql-results-in-new-frame to t for this to occur.
;;
;;  When results buffers appear in new frames, you can set the size infor-
;;  mation by setting sql-resize-results-frames to t, and
;;  sql-results-frame-width and sql-results-frame-height to appropriate
;;  values.
;;
;;  ABBREVS
;;
;;  You can use abbrevs in sql-mode, sql-batch-mode and
;;  sql-interactive-mode by changing the value of  sql-abbrev-mode to a
;;  non-nil value (it is nil by default).  There are a default set of
;;  abbrevs, but you can customize them by the existance of a file:
;;
;;      $HOME/.sql-abbrevs
;;
;;  This file should be created by M-x write-abbrev-file, or something
;;  similar.  See the help on abbrev-mode, write-abbrev-file, and
;;  define-abbrev-table for details.
;;
;;  There is a default set of abbrevs that map common keywords to one and
;;  two-letter keystrokes.
;;
;;  FONT LOCKING
;;
;;  Font-lock-mode can be turned on by setting the variable
;;  sql-font-lock-buffers to a non-nil value.  You can select which types
;;  of buffers you wish to font lock.  If you want them all to be
;;  font-locked set this variable to 'all.  If you would only like a
;;  subset of the four modes to be font-locked, set this variable to a
;;  list of the mode types you wish to font-lock.  For example, to
;;  font-lock only sql-mode and sql-interactive-mode buffers, add this
;;  to your $HOME/.sql-mode file.
;;
;;      (setq sql-font-lock-buffers '(sql-mode sql-interactive-mode))
;;
;;  See the variable sql-mode-font-lock-keywords for details on what will
;;  get font-locked.  By default, it is assumed that you are using a light
;;  background.  If you are using a dark background, set the variable
;;  sql-video-type to 'inverse.  If you are using a monochrome monitor,
;;  set sql-vidoe-type to 'monochrome (see the section Customization).
;;
;;  To change the faces that are used in font-lock-mode, you will need to
;;  set the values of the face variables in your $HOME/.sql-mode file.  An
;;  example, if you want conjunctions to appear blue, include thiese lines:
;;
;;      (make-face 'sql-conjunction-face)
;;      (set-face-foreground 'sql-conjunction-face "blue")
;;
;;  The face names are: sql-query-face, sql-set-face, sql-special-face,
;;                      sql-conjunction-face, sql-sysadm-face,
;;                      sql-aggregate-face, sql-prompt-face,
;;                      and sql-results-face.
;;
;;  To change the regexp that searches for words to font-lock, you will have
;;  to set the value of the variable sql-mode-font-lock-keywords.  See the
;;  help on the variable font-lock-keywords for more details.
;;
;;  GLOBAL COMMAND HISTORY
;;
;;  If you want to execute a command in one sql-batch-mode buffer that
;;  has already been entered in another sql-batch-mode buffer, you can
;;  access it by browsing the global command history.  The last 40
;;  commands evaluated in *any* sql-batch-mode buffer are saved in the
;;  global command history.  To insert the previous global command in
;;  the current buffer, simply invoke sql-previous-global-history
;;  (bound to M-P (that's meta-shift-p) by default).  Similarly, the
;;  next global history element is accessed via sql-next-global-history
;;  (bound to M-N by default).
;;
;;  This can be a useful way to execute similar commands in different
;;  batch mode buffers.
;;
;;  TOP TEN LIST
;;
;;  If you are commonly executing a small set of SQL commands, you can save
;;  them in a top ten list, and later reference them easily.  The commands
;;  are saved in the file $HOME/.sql-top-ten.  To save a common command to
;;  your list, invoke sql-add-top-ten (bound to C-c t by default), which
;;  will save the contents of the current sql-batch-mode buffer into one
;;  of the ten positions available.  To later access a top ten command,
;;  invoke sql-insert-top-ten<n>, where n is a number between 0 and 9.
;;  These functions are bound by default to C-c 0 through C-c 9.
;;
;;  Your top ten list is saved automatically every time you make an
;;  addition to it.
;;
;;  If you have any entries in your top ten list, and you are using XEmacs
;;  version 19.12 or later, a top ten toolbar will appear on the left hand
;;  side of the frame.  If you do not want this toolbar to appear, a simple
;;  menu selection from the options menu will turn it off/on.
;;
;;  SQL ERRORS
;;
;;  The commands sql-next-error and sql-previous-error (bound to C-c n and
;;  C-c p in sql-batch-buffers by default) make it easy to go to the line
;;  of SQL code on which the error occured.  These functions work similarly
;;  to next-error and previous-error.
;;
;;  MOUSE-BASED COMPLETION
;;
;;  If you click with the right mouse button on a completable word in a
;;  sql-batch-mode buffer, it will pop up a list of completions for that
;;  word.  This makes it easy to change a column in a table query, for
;;  instance.
;;
;;  If the mouse pointer is not over a completable word, then the
;;  association list is popped up in a menu.
;;
;;  WORD YANKING
;;
;;  Clicking on the middle mouse button in a results buffer will yank
;;  the word under point up into the batch-mode buffer.  This makes it
;;  easy to create queries based on results shown in the results
;;  buffer.  Quotes go around values that need it, and column names
;;  are inserted as necessary.
;;
;;  If you don't want the column name or quotes, hold down the shift
;;  key and click the middle mouse button.  This will simply copy the
;;  word under the mouse into the batch buffer.
;;
;;  PREVIOUS MATCHING HISTORY
;;
;;  Have you ever though to yourself: "Where's that update command
;;  that I ran about 30 queries ago?" Well, now you'll be able to
;;  find it more easily with the function sql-previous-matching-history
;;  (bound to M-m by default).  In this case do:
;;
;;      M-m update RET
;;
;;  TOOLBAR SUPPORT
;;
;;  Toolbar support for XEmacs version 19.11 has been dropped.  It was
;;  based on a package called toolbar.el, which had some problems.  In
;;  XEmacs 19.12, "real" toolbars were introduced, and SQL Mode tries
;;  to make good use of them.  You can turn the toolbars on/off from
;;  the Options menu.
;;
;;  ROW INSERTION
;;
;;  The function sql-insert-row is an interactive function that guides
;;  you through an insert into a table.  It prompts for the table name
;;  and a value for each column, and then builds an insert statement,
;;  putting quotes around fields that need them.
;;
;;  BCP SUPPORT
;;
;;  SQL Mode now supports the functions sql-bcp-out and sql-bcp-in.
;;  Try them, they're really easy to use.
;;
;;  LOADING OF STORED PROCEDURES
;;
;;  sql-load-sp is a new function that will load a stored procedure.
;;  There are options for configuring it as you need, but by default
;;  the stored procedure file is run through the C preprocessor,
;;  loaded into the current buffer, and then sent to the SQL server.
;;
;;  ASYNCHRONOUS BUFFER EVALUATION
;;
;;  Buffer evaluation can now be done in the background.  The function
;;  sql-evaluate-buffer-asynchronous is bound to M-e by default.  After
;;  evaluating a buffer in the background, you should be able to continue
;;  to use XEmacs normally.  The variable sql-finished-query-options
;;  determines how SQL Mode will notify you when the evaluation completes.
;;
;;  NOTE: If your query is output-intensive, you probably still won't be
;;  able to use XEmacs fully until the query finished, because XEmacs
;;  will be busy putting the output in the results buffers.
;;
;;  If you find yourself doing background evaluation all the time, set
;;  the value of the variable `sql-evaluation-method' to 'background.
;;  You can set this variable from the menubar by selecting the item
;;  `Options-->Evaluation Method'.  The value of this variable will
;;  affect the keybindings M-i and C-c C-e, as well as the "GO" button
;;  from the toolbar.  If you want to do most of your evaluations in
;;  the foreground, but occasionally run a background query, you can
;;  invoke the command `sql-evaluate-buffer-asynchronous' directly
;;  (bound to the M-e key by default).
;;
;;  TYPE TO UPDATE
;;
;;  If you perform a simple query on a table like:
;;  
;;      select * from table1 where column1 = value1
;;
;;  You can then type directly into the results buffer to change the data
;;  in a particular row, and SQL Mode will determine what has changed,
;;  and perform the appropriate update.
;;
;;  The specific steps to do this are as follows:
;;  
;;  1. Perform the select
;;  2. Tell SQL Mode that you want to edit the data by either typing
;;     `u' or clicking the update button on the toolbar.
;;  3. You are now in sql-results-edit-mode.  The keybindings have
;;     returned to normal.  You are in overtype mode to facilitate
;;     the keeping of the alignment of the columns.  Edit the text
;;     as you need to.  You may edit multiple rows of data.
;;  4. Type M-i or C-c C-e to build the update statement.
;;  5. Review the update statement in the batch buffer.
;;  6. Evaluate the update statement by doing a M-i (or C-c C-e).
;;
;;  See the help on the functions sql-edit-row and sql-update for
;;  more details.
;;
;;  WARNING: SQL Mode relies on the alignment of the columns, so you
;;           have to be very careful when typing, and additionally the 
;;           columns have to be lined up to begin with, otherwise you
;;           will lose.
;;
;;  CACHE SAVING AND LOADING
;;
;;  Completion is a nice feature, but the most common complaint from
;;  people who don't use it is that it's too slow.  Now it is possible
;;  (and in fact the default) to save the cached data between Emacs
;;  sessions.  When you exit SQL Mode (by killing the buffer or exiting
;;  Emacs) the cached data is saved to disk in the directory
;;  ~/.sql-cache-dir (you can, of course, tell SQL Mode to save it
;;  somewhere else).  The next time you start SQL Mode it will load the
;;  cache file, and you won't have to wait for completions.
;;
;;  The one drawback to this scheme is when ddls change, or new tables,
;;  stored procedures, or databases are added.  The solution in these
;;  cases is to clear the cached data.  There are menu items under the
;;  Actions menu that enable you to clear all the cached data, or any
;;  specific cache that you like.
;;
;;  The cache data is saved in a file that is based on the current
;;  server and database in the ~/.sql-cache.dir directory.
;;
;;  DELAYED COMPLETION
;;
;;  As more and more functions require table names or column names
;;  as arguments, there was a growing need for "delayed"
;;  completion.  If SQL Mode prompts you for a table or column
;;  name, you will be able to type immediately if you don't need
;;  completion.  If you do wish to see a completion list, then hit
;;  TAB or SPACE, at which point SQL Mode will go to the server to
;;  get the information it needs (if it isn't cached already).
;;
;;  Along with saving the cache data, this feature should serve to make
;;  completion more pleasant and more useful.
;;
;;  DELETE, CUT, COPY, AND PASTE FUNCTIONALITY
;;
;;  Results buffers now have some added functionality to help move
;;  data around.  If you wish to delete a row from the database, simply
;;  type C-k on the row you wish to delete.  Deleting multiple rows is
;;  as simple as highlighting the rows and hitting C-w.  The expected
;;  functionality for delete, cut, copy, and paste all have a database
;;  slant to them when invoked in the results buffer, but are bound to
;;  the keys you would expect in Emacs.
;;
;;  There is a special SQL Mode clipboard for storing data between cut,
;;  copy, and paste operations.
;;
;;  sql-delete-row, sql-delete-region:
;;      Deletes the row that point is on, or all rows in the region.
;;      The data is *not* copied to the clipboard.
;;
;;  sql-cut-row, sql-cut-region:
;;      Deletes the row that point is on, or all rows in the region.
;;      The data *is* copied to the clipboard.
;;
;;  sql-copy-row, sql-copy-region:
;;      Copies the row that point is on, or all rows in the region, to
;;      the clipboard.  Does not do any database manipulation.
;;
;;  sql-paste:
;;      Pastes the data from the clipboard into the batch buffer by way
;;      of an insert statement.
;;
;;  PREFERED EVALUATION METHOD
;;
;;  If you find yourself doing sql-evaluate-buffer-background more
;;  often than it's synchronous counterpart, there is a variable that
;;  will control the behavior of the "GO" button.  Set the variable
;;  sql-evaluation-method to either 'foreground or 'background.
;;
;;  BCP SINGLE ROW
;;
;;  The function sql-bcp-row-out fakes a bcp out of the database, but
;;  what it really does is to parse the results buffer and put a "|"
;;  between columns and then write the result to a file.  If all goes
;;  well, you should be able to then bcp this file into another
;;  database as if it was a real bcp file.  This makes it easy to copy
;;  out just one row.
;;
;;  STAY LOGGED INTO THE DATASERVER
;;
;;  By setting the value of the variable `sql-stay-logged-in' to a
;;  non-nil value (or by selecting the menu item
;;  `Options-->Dataserver Connection-->Stay Logged In') you can keep an
;;  open connection to the dataserver to do your queries.  This
;;  increases the performance of almost all aspects of SQL Mode
;;  dramatically, as you remove the overhead of invoking a new isql
;;  process, and logging in.
;;
;;  ASSOCIATED EVALUATION BUFFERS FOR SQL-MODE BUFFERS
;;  If you edit a file in sql-mode, you can now send conents of
;;  that file to a sql-batch-mode buffer for evaluation.  You can
;;  specify the batch mode buffer to perform the evaluations by
;;  selecting "SQL-->Set Evaluation Buffer" from the menu bar.
;;  Once an evaluation buffer has been set, you can send it the
;;  contents of the whole buffer with `sql-mode-evaluate-buffer'
;;  (bound to M-i), `sql-mode-evaluate-region', and
;;  `sql-mode-evaluate-statement'.  The function
;;  `sql-mode-evaluate-statement' will operate on the current
;;  block of SQL code, inbetween the preceeding "go" and up to and
;;  including the next "go".
;;  
;;  HISTORY SAVING AND LOADING
;;
;;  Your history can now be saved between emacs sessions.  Just
;;  set the value of the variables `sql-save-history-on-exit' and
;;  `sql-always-load-history' to t (you can do this from the Options
;;  menu as well).  The history information is saved to files in the
;;  ~/.sql-history-dir directory.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mailing Lists:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  If you would like to join the beta testers list, or the sql-mode
;;  discussion mailing list, send add/drop requests to
;;  sql-mode-request@atlantic2.sbi.com.  Discussions can be sent to
;;  sql-mode-discuss@atlantic2.sbi.com.  Bug reports and enhancement
;;  requests should still be sent to sql-mode-help@atlantic2.sbi.com only
;;  (see below for how to submit a bug report).
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bug Reports:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  You can submit a bug report by typing M-x submit-bug-report or
;;  selecting `Submit Bug Report' from the menu bar.  Try to be as specific
;;  as possible in the description of your problem.
;;
;;  Similarly, enhancement requests can be submitted by typing
;;  M-x submit-enhancement-request or selecting `Submit Enhancement Request'
;;  from the menu bar.
;;
;;  A mail buffer will be set up with some information that will make it
;;  easier to diagnose the problem.  In addition, to To: field will be
;;  filled out for you.  Please don't send bug reports and enhancement
;;  requests to my personal account, as I try to keep them separate from my
;;  `real' work.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Known Bugs:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  o Indentation.  It is in a horrible state.
;;  o False `where clause' warning messages (perhaps fixed when the
;;    variable `sql-risky-searches' is set to t)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Future Enhancements:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Things I'd like to see in SQL Mode: (in approximate order of importance)
;;
;;  If you would like to volunteer to help implement any of these items,
;;  please send mail to sql-mode-help@atlantic2.sbi.com.
;;
;;  * Accurate indentation of SQL code
;;      - syntax parser (good for more than just indentation)
;;      - indentation engine (based on cc-mode.el?)
;;  * Support for Sql*Plus, mSQL, Postgress95, and any other sql really
;;  * An Info page
;;  * Accurate `where' warnings (right now there are some false positives)
;;      - this may be fixed by setting sql-risky-searches to `t'
;;  o Exit on termination of an interactive process
;;  o Enhanced printing of results buffers (split pages) to printer
;;      - portrait and landscape modes
;;  * A more accurate sql-get-completion-context
;;  o Toolbars should use transparent backgrounds
;;  * Split the file sql-mode.el into more manageable chunks
;;  o sql-save-window-configuration
;;  o fix sql-frame-icon
;;  o mnemonics in top-ten menu
;;
;;  `*' means that the item is in progress, `o' means that it isn't (yet)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Revision History:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;			    Version 0.922
;;
;;  19-JAN-96 (pez) - changed the default value of sql-risky-searches to t
;;
;;  15-NOV-95 (pez) - renamed sql-bcp-row-out to sql-fake-bcp-out-row
;;                  - added sql-fake-bcp-out and sql-fake-bcp-out-region
;;
;;  09-NOV-95 (pez) - removed sql-use-mnemonic-only-in-menubar, since it
;;                    wasn't referenced (and is obsolete due to mnemonic
;;                    aliases)
;;
;;  03-NOV-95 (pez) - added value completion
;;
;;  02-NOV-95 (pez) - removed NULL from sql-keyword-regexp
;;
;;  31-OCT-95 (pez) - re-wrote sql-evaluate-buffer-asynchronous
;;                  - greatly enhanced sql-magic-yank-under-point
;;                  - added sql-operators and operator completion
;;
;;  27-OCT-95 (pez) - added user completion
;;                  - added buffer info for sql-mode buffers
;;
;;  25-OCT-95 (pez) - added menu aliases
;;                  - added sql-evaluation-buffer for sql-mode buffers
;;                    for batch evaluation from sql-mode buffers
;;                  - added sql-evaluate-buffer for sql-mode buffers
;;                  - added sql-evaluate-region for sql-mode buffers
;;                  - added sql-evaluate-statement for sql-mode buffers
;;                  - increased performance for sql-paste, sql-delete-row
;;
;;  24-OCT-95 (pez) - re-wrote sql-get-rows-affected function
;;                  - cleaned up sql-modeline-format variable, added
;;                    process status
;;                  - finally made history local to the batch buffer
;;                  - added sql-save-history-on-exit
;;                  - added sql-always-load-history
;;                  - added history element number to modeline
;;
;;  23-OCT-95 (pez) - added "stay logged into the server" support
;;                  - dramatically sped-up all completion
;;                  - added sql-log-in-on-start variable
;;                  - added sp_helpdb completion
;;                  - added table alias completion for columns
;;                  - fixed the code that inserted lines in the user's
;;                    .emacs file to append to the load-path variable
;;                  - added messages while building insert statements
;;                    since they can take a while
;;
;;  17-OCT-95 (pez) - added support for buffer-modified-p in sql-batch-mode
;;                  - cleaned up font-lock support for results buffers
;;
;;  16-OCT-95 (pez) - added sql-top-ten-help vector for echoed toolbar help
;;                    strings
;;
;;			    Version 0.921
;;
;;  15-OCT-95 (pez) - added top ten mnemonics
;;                  - top ten item now saves value of point
;;                  - added sql-top-ten-execute vector
;;
;;  28-SEP-95 (pez) - results buffer appears at start
;;                  - big-menubar in results buffer as well
;;                  - enhanced toolbar to include bcp in, bcp out, cut,
;;                    copy, paste and delete icons
;;
;;  16-AUG-95 (pez) - added sql-results-toolbar
;;
;;  09-AUG-95 (pez) - changed all screen references to frame
;;                  - fixed sql-results-in-new-frame behavior
;;                  - added group by column completion
;;                  - added completion for field list (i.e. select [TAB])
;;
;;  07-AUG-95 (pez) - added automagical load-path handling with sql-test.el
;;
;;  03-AUG-95 (pez) - removed sql-loaded variable (it was useless)
;;
;;  24-JUL-95 (pez) - enhanced sql-current-buffer-info to show caches
;;
;;  21-JUL-95 (pez) - if region is active sql-comment-line-toggle invokes
;;                    sql-comment-region-toggle
;;
;;  18-JUL-95 (pez) - changed most completion to be "delayed"
;;                  - added delete, cut, copy, and paste functionality
;;
;;  14-JUL-95 (pez) - added cache saving and loading
;;
;;  13-JUL-95 (pez) - added sql-delete-row
;;                  - added variable sql-preferred-evaluation-method
;;                  - added hook sql-init-hook
;;
;;  12-JUL-95 (pez) - added sql-bcp-row-out
;;
;;  11-JUL-95 (pez) - added sql-top-ten-toolbar
;;                  - changed menu to have "Emacs" and "SQL" buttons when
;;                    you use the big menubar
;;                  - changed the default value of sql-use-big-menus to t
;;                  - changed the value of sql-bcp-switches
;;
;;			    Version 0.919
;;
;;  22-MAY-95 (pez) - added association completion
;;
;;  14-MAY-95 (pez) - enhanced sql-*yank-under-point functions to work from
;;                    any buffer, including the minibuffer
;;
;;  12-MAY-95 (pez) - printing enhancements: sql-print-command,
;;                    sql-print-switches, enscript auto detection
;;                  - changed sql-results-hook to sql-results-mode-hook
;;                  - added hook variable devfars
;;                  - added database completion
;;
;;  10-MAY-95 (pez) - added sql-finished-query-options
;;                  - fixed sql-stay-in-batch-buffer bug
;;                  - finally made completion work in interactive mode
;;                    buffers
;;
;;  08-MAY-95 (pez) - fixed cleanup of results buffers so that the
;;                    marking-changes functionality isn't in effect
;;
;;  05-MAY-95 (pez) - added sql-stay-in-batch-buffer variable
;;                  - wrote sql-evaluate-buffer-background and related
;;                    functions
;;                  - modified the behavior of sql-no-warn so that it
;;                    appends to the user's ~/.sql-mode file rather than
;;                    creating a new file
;;                  - changed the default binding of C-L from sql-recenter
;;                    to sql-reposition-windows (C-l is still sql-recenter)
;;
;;  01-MAY-95 (pez) - made sql-*-history functions work from results buffers
;;                  - added canned updates (sql-edit-row and sql-update)
;;
;;  26-APR-95 (pez) - fixed sql-insert-row to pay attention to column types
;;                  - added type-to-update functionality
;;                  - enhanced sql-get-columns to get the column type
;;
;;  14-APR-95 (pez) - added sql-bcp-out and sql-bcp-in functions
;;                  - updated big menubar to include much more stuff
;;                  - added sql-magic-yank-under-point
;;
;;  02-APR-95 (pez) - added sql-yank-under-point
;;                  - changed sql-set-database so that it clears cached
;;                    data
;;
;;  31-MAR-95 (pez) - changed sql-evaluate-buffer so that it doesn't
;;                    destroy and re-create results buffers every time
;;                  - added sql-insert-row
;;
;;  17-MAR-95 (pez) - fixed sql-end-of-buffer and sql-beginning-of-buffer
;;                    to push the mark
;;
;;  07-MAR-95 (pez) - split up toolbar code for XEmacs version 19.11 and
;;                    19.12 since it's so different
;;
;;  27-FEB-95 (pez) - made popup on button3 be context sensitive, displaying
;;                    completion lists where appropriate
;;                  - added sql-load-hook
;;
;;  24-FEB-95 (pez) - enhanced sql-get-completion-context to understand
;;                    commas, as in: update a set b=5, c=3
;;
;;  23-FEB-95 (pez) - added sql-load-sp funciton
;;                  - fixed sql-help-for-help to hande button events
;;                  - fixed font-lock regexps to match words
;;                  - fixed sql-determine-video-type
;;
;;  09-FEB-95 (pez) - made isearch interactively recenter horizontally
;;                    thanks to code written by Barry Warsaw
;;
;;  08-FEB-95 (pez) - added sql-previous-matching-history
;;
;;  06-FEB-95 (pez) - wrote sql-toolbar.el (based on toolbar by Andy Piper)
;;                    and related functions
;;                  - designed sql-icons.el
;;                  - fixed sql-goto-history
;;
;;  30-JAN-95 (pez) - added sql-other-window-done
;;
;;  06-JAN-95 (pez) - added sql-split-window-horizontally and related
;;                    functions
;;
;;  28-DEC-94 (pez) - added FSF emacs support
;;
;;  15-DEC-94 (pez) - fixed history/next-error bug
;;
;;  13-DEC-94 (pez) - made sql-stored-procedure-list buffer-local and added
;;                    it to sql-clear-cached-data (these were bugs)
;;
;;  02-DEC-94 (pez) - added sql-next-error and sql-previous-error
;;                  - fixed header-motion/history incompatibility
;;
;;  30-NOV-94 (pez) - added sql-read-interfaces-file
;;                    added sql-set-sybase
;;
;;  29-NOV-94 (pez) - added sql-insert-gos and sql-insert-semi-colons
;;                  - fixed column name completion bug
;;
;;  28-NOV-94 (pez) - added sql-evaluate-buffer-hook
;;                    added sql-exit-sql-mode functions
;;
;;  25-NOV-94 (pez) - fixed sql-drag-display to work with shifting headers
;;
;;  18-NOV-94 (pez) - added global history
;;                  - changed keybindings in sql-results-mode to be closer
;;                    to view-mode
;;                  - added edit mode in sql-results-mode
;;                  - improved header management (no moving of headers if
;;                    there are multiple headers)
;;                  - added top-ten queries
;;
;;  15-NOV-94 (pez) - added scroll-in-place (with header management)
;;                  - changed sql-intersperse-headers to nil
;;
;;  08-NOV-94 (pez) - enhanced column completion to work after `and' and
;;                    `or' clauses
;;                  - fixed regexps for warnings
;;
;;  02-NOV-94 (pez) - made font-lock changes work `on the fly'
;;                  - added monchrome font-lock settings
;;                  - enhanced associations to use databases as well
;;
;;  31-OCT-94 (pez) - cleaned up header comments
;;
;;  28-OCT-94 (pez) - added column completion (first crack at it)
;;                  - header interspersion
;;                  - made keyword completion and font-lock regexps case-
;;                    insensitive
;;                  - fixed poor completion behavior
;;
;;  29-SEP-94 (pez) - linked batch-mode and results-mode buffers
;;
;;  17-AUG-94 (pez) - cleaned up commented-out code
;;                  - added frame-icon-title-format cusomization
;;
;;  25-JUL-94 (pez) - added sql-interactive-command-switches
;;                  - fixed sql-end-of-row for sql-interactive-mode
;;
;;  21-JUL-94 (pez) - linked history of batch buffers
;;                  - changed modeline-format
;;                  - made results buffers resizable
;;
;;  20-JUN-94 (pez) - added save current options
;;                  - added change highlighting in results buffers
;;                  - fixed some options bugs
;;
;;  16-JUN-94 (pez) - added options menu
;;
;;  08-JUN-94 (pez) - added saving and loading of history
;;
;;  07-JUN-94 (pez) - fixed heinous font-lock bug
;;                  - added more font-lock customizability
;;                  - made completion more flexible
;;                  - made menus version-dependant
;;
;;  24-MAY-94 (pez) - added keyword completion
;;                  - added database buffer-local variable
;;                  - added sql-current-buffer-info
;;
;;  23-MAY-94 (pez) - added font-locking for results buffers
;;                  - made associations work in interactive buffers
;;                  - added table completion
;;
;;  16-MAY-94 (pez) - made isearch-mode-end-hook buffer local in
;;                    sql-interactive-mode and sql-results-mode buffers
;;
;;  12-MAY-94 (pez) - added minibuffer completion for servers and users
;;
;;  09-MAY-94 (pez) - changed buffer names to be consistent with emacs
;;                    conventions
;;                  - made enhancements to sql-previous-history and
;;                    sql-next-history
;;
;;  08-MAY-94 (pez) - broke sql-mode out into sql-mode, sql-batch-mode,
;;                    and sql-interactive-mode
;;                  - added flashing messages
;;
;;  06-MAY-94 (pez) - added full font-lock support (thanks to J. Price)
;;                  - made font-lock colors customizable
;;                  - history bug fixes
;;
;;  02-APR-94 (pez) - changed function comments as per RMS's request
;;
;;  02-MAR-94 (pez) - added warnings for where clauses and rows affected
;;                  - added command history
;;	            - made sql-results-mode separate from the function
;;		      sql-evaluate-buffer
;;
;;  01-MAR-94 (pez) - added a first attempt at automatic indentation of
;;                    SQL code
;;
;;  25-FEB-94 (pez) - added drag scrolling
;;		    - added the following functions:
;;				sql-set-server
;;				sql-set-user
;;				sql-set-password
;;				sql-set-sql-command
;;
;;  22-FEB-94 (pez) - made sql-command customizable
;;		    - updated font-locking to be more customizable
;;		    - added sql-abbrev-mode
;;		    - changed the file name to sql-mode.el
;;
;;  18-FEB-94 (pez) - added popup association menu
;;
;;  17-FEB-94 (pez) - made password entry invisible (customizable)
;;		    - removed autogenerated passwords (bad algorithm)
;;		    - added associations
;;		    - fixed new-frame logic
;;		    - added resizing of new frames
;;
;;  11-FEB-94 (pez) - added comment customization
;;		    - added the following variables:
;;				sql-comment-regions-by-line
;;				sql-scroll-overlap
;;				sql-max-frame-width
;;    				sql-save-all-results
;;		    - fixed sql-beginning-of-row
;;		    - fixed sql-end-of-row
;;		    - created separate menus for sql-mode buffers and 
;;		      sql-results buffers
;;		    - added support for font-lock-mode
;;		    - added results in new frame
;;		    - added results in new frame toggle function
;;		    - added sql-mode hook and sql-results-hook
;;
;;  07-FEB-94 (pez) - added the following functions:
;;				sql-forward-column		C-F
;;				sql-backward-column		C-B
;;				sql-beginning-of-row		C-a
;;				sql-end-of-row			C-e
;;				sql-pop-and-rename-buffer
;;		    - added region commenting
;;		    - added sql-mode-syntax-table
;;		    - added sql-menu (for lemacs 19.x)
;;		    - added bug-reporting mechanism
;;
;;  01-FEB-94 (pez) - changed the file name to isql-mode.el (from isql.el)
;;                  - added line and buffer commenting
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Acknowledgements:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  SQL Mode would not be nearly the package it is today without a lot of
;;  help from other people.  First of all I'd like to thank Jim Nelson for
;;  writing the original isql.el, which was the inspiration for early
;;  versions of sql-mode.el (and subsequently the package SQL Mode).
;;
;;  The code for sql-get-password and related functions was copied whole-
;;  sale from Andy Norman's ange-ftp package.
;;
;;  The code for sql-submit-bug-report was based heavily on Kyle Jones'
;;  vm package.
;;
;;  The code for drag scrolling was written by Martin Boyer, Hydro Quebec,
;;  and modified by Laurent Langlois.
;;
;;  The regexps for font-lock-mode were written by James "Clam" Price,
;;  Salomon Brothers, Inc.
;;
;;  The code for sql-restore-window-config was shamelessly lifted from
;;  comint.el.
;;
;;  The sql-isearch-begin and sql-isearch-end functions were inspired by
;;  code written by Barry Warsaw.
;;
;;  There are many other small snippets of code throughout SQL Mode
;;  that have been pilfered from the many excellent elisp packages written
;;  for XEmacs.
;;
;;  Many thanks to the members of the help-xemacs@cs.uiuc.edu (formerly
;;  help-lucid-emacs@lucid.com) mailing list, the members of the CDTS team
;;  at Salomon Brothers, and James "Clam" Price.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Getting the latest version:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Requests can be sent to sql-mode-help@atlantic2.sbi.com to receive the
;;  latest version of sql-mode.el.  It is actively being enhanced (and
;;  fixed).
;;
;;  If you work for Salomon, Inc. you can use anonymous ftp to get the
;;  latest version.  It is on atlantic2 in the directory /pub/sql-mode.
;;
;;  For information on getting SQL Mode from outside the sbi.com domain,
;;  see the NEWS file that should have come with the distribution.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar sql-lucid (string-match "Lucid" (emacs-version)))

(defvar sql-xemacs (string-match "XEmacs" (emacs-version)))

(defvar sql-xemacs-19-12 (and sql-xemacs
			      (or (> emacs-major-version 19)
				  (and (eq emacs-major-version 19)
				       (> emacs-minor-version 11)))))

(or sql-lucid
    (progn
      (defun buffer-live-p (b)
	(and (bufferp b) (buffer-name b)))
      (defun redraw-modeline ()
	(set-buffer-modified-p (buffer-modified-p)))))

(require 'comint)

(or (featurep 'scroll-in-place)
    (condition-case nil
	(progn
	  (require 'scroll-in-place)
	  (setq scroll-in-place nil))
      (error (progn
	       (defun scroll-up-in-place (arg)
		 (interactive)
		 (scroll-up arg))
	       (defun scroll-down-in-place (arg)
		 (interactive)
		 (scroll-down arg))
	       (setq sql-global-move-headers nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User Definable Variables:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar sql-mode-hook nil
  "*Function or functions to run on entry to sql-mode.")

(defvar sql-batch-mode-hook nil
  "*Function or functions to run on entry to sql-batch-mode.")

(defvar sql-interactive-mode-hook nil
  "*Function or functions to run on entry to sql-interactive-mode.")

(defvar sql-results-mode-hook nil
  "*Function or functions to run on entry to sql-results-mode.")

(defvar sql-before-evaluate-buffer-hook nil
  "*Function or functions to run just before the buffer is evaluated.")

(defvar sql-evaluate-buffer-hook nil
  "*Function or functions to run after the buffer is evaluated.")

(defvar sql-load-hook nil
  "*Function or functions to run after sql-mode.el is loaded.")

(defvar sql-init-hook nil
  "*Function or functions to run after SQL Mode is initialized.")

(defvar sql-dataserver-type 'sybase
  "Type of database you are using.  Recognized options are 'sybase,
'oracle 'mSQL and 'postgress.  Setting this variable will change the default
value of a few variables, as well as the behavior of SQL Mode.
Oracle and mSQL support should be considered limited compared to Sybase
support.  In fact, support for vendors other than Sybase is just about
nil right now, although it is high on my priority list for SQL Mode.
If you would like to help with this support, *please* let me know.

Do not set the value of this variable directly.  Instead use the function
`sql-set-dataserver-type'.")

(defvar sql-command
  (cond ((eq sql-dataserver-type 'sybase)
	 "isql")
	((eq sql-dataserver-type 'oracle)
	 "sqlplus")
	((eq sql-dataserver-type 'mSQL)
	 "mSQL")
	((eq sql-dataserver-type 'postgress)
	 "psql")
	(t
	 "isql"))
  "*Command to invoke sql.
This should be just the program name, NOT path/command.  The command
should be in your path.  If you need to add switches, see the variables
`sql-batch-command-switches' and `sql-interactive-command-switches'.
The value of this variable will have a default based on the value of
the variable `sql-dataserver-type'.  If you are changing the value of this
variable you should first see the variable `sql-dataserver-type' and
the function `sql-set-dataserver-type'.")

(defvar sql-batch-command-switches nil
  "*Switches to concatenate to the sql-command in sql-batch-mode.")

(defvar sql-interactive-command-switches nil
  "*Switches to concatenate to the sql-command in sql-interactive-mode.")

(defvar sql-bcp-command "bcp"
  "*Command to invoke bcp.
This should be just the program name, NOT path/command.  The command
should be in your path.  If you need to add switches, see the variables
`sql-bcp-command-switches'.")

(defvar sql-bcp-command-switches nil
  "*Switches to concatenate to bcp when running sql-bcp-out or sql-bcp-in.
If the value of this variable is nil (the default), then the following
switches are used: -c -t| -r\n")

(defvar sql-bcp-user nil
  "*User name to use for bcp commands.
If non-nil, all bcp commands will be of the form database.SQL-BCP-USER.table")

(defvar sql-evaluation-method 'foreground
  "*The preferred way to call sql-evaluate-buffer.
Possible options are currently 'foreground, which will block until the
query returns, and 'background, which will evaluate the query in the
background.

The toolbar GO button pays attention to this variable.")

(defvar sql-require-final-go t
  "*Append `go' to the end of the buffer before evaluation if non-nil.")

(defvar sql-secure-passwords t
  "*Make password entry invisible if non-nil.")

(defvar sql-abbrev-mode nil
  "*Invoke abbrev-<minor>-mode if non-nil.")

(define-abbrev-table 'sql-mode-abbrev-table
  '(("s" "select")
    ("f" "from")
    ("w" "where")
    ("l" "like")
    ("u" "update")
    ("ob" "order by")
    ("tr" "transaction")
    ("st" "statistics")))

(defvar sql-video-type nil
  "*The type of video display you are using.  It should be one of
	'regular	for light backgrounds
	'inverse	for dark backgrounds
	'monochrome	for monochrome monitors

If the value of this variable is nil, it will try to guess the appropriate
value in the function `sql-determine-video-type'.")

(defvar sql-minibuffer-status t
  "*This variable is obsolete.  Please see `sql-finished-query-options'.")

(defvar sql-comment-start-regexp "[/][*][ ]"
  "Regexp to match sql-comment-start-string.")

(defvar sql-comment-start-string "/* "
  "Start string to insert in order to comment-out SQL code.")

(defvar sql-comment-end-regexp "[ ][*][/]"
  "Regexp to match sql-comment-end-string.")

(defvar sql-comment-end-string " */"
  "End string to insert in order to comment-out SQL code.")

(defvar sql-resize-results-frames t
  "*Resize results frames according to sql-results-frame-width and
sql-results-frame-height if non-nil.")

(defvar sql-results-frame-width 100
  "*Width of new results frames generated by sql-batch-mode if
sql-resize-results-frames is non-nil.")

(defvar sql-results-frame-height 25
  "*Height of new results frames generated by sql-batch-mode if 
sql-resize-results-frames is non-nil.")

(defvar sql-max-frame-width 5000
  "*Maximum frame width for sql-results buffers.")

(defvar sql-scroll-overlap 2
  "*Number of columns to overlap when scrolling to the end of the row.")

(defvar sql-save-all-results nil
  "*Replace results bufffers as they are generated if nil, save all
results buffers otherwise.")

(defvar sql-results-in-new-frame nil
  "*When non-nil, place results in a new frame, otherwise place
results in the bottom half of the window.")

(defvar sql-batch-in-new-frame nil
  "*When non-nil, place new sql-batch-mode buffers in their own frame.")

(defvar sql-resize-batch-frames t
  "*Resize batch frames according to sql-batch-frame-width and
sql-batch-frame-height if non-nil.")
  
(defvar sql-batch-frame-width 100
  "*Width of new batch frames if sql-resize-batch-frames is non-nil.")

(defvar sql-batch-frame-height 40
  "*Height of new batch frames if sql-resize-batch-frames is non-nil.")

(defvar sql-interactive-in-new-frame nil
  "*When non-nil, place new sql-interactive-mode buffers in their own frame.")

(defvar sql-resize-interactive-frames t
  "*Resize interactive frames according to sql-interactive-frame-width and
sql-interactive-frame-height if non-nil.")
  
(defvar sql-interactive-frame-width 80
  "*Width of new interactive frames if sql-resize-interactive-frames is
non-nil.")

(defvar sql-interactive-frame-height 40
  "*Height of new interactive frames if sql-resize-interactive-frames is
non-nil.")

(defvar sql-comment-regions-by-line t
  "*Setting sql-comment-regions-by-line to a non-nil value will comment out
each line when sql-comment-region is (or related functions are) invoked.
Setting sql-comment-regions-by-line to nil will cause a single
sql-comment-start-string and sql-comment-end-string to be inserted, 
regardless of the region size.")

(defvar sql-comment-buffer-ignore-lines 0
  "*Number of lines at the top of the buffer to ignore whe commenting and
un-commenting with sql-comment-buffer functions.")

(defvar sql-font-lock-buffers (if sql-lucid 'all nil)
  "*When non-nil, invoke font-lock-mode on selected sql-related mode buffers.
To specify buffers to fontify, set to 'all for all buffers, or a list of
which types of buffers, from 'sql-mode, 'sql-batch-mode, 'sql-interactive-mode
and 'sql-results-mode.

For example, to font lock sql-mode buffers and sql-batch-mode buffers, but not
sql-interactive-mode buffers or sql-results buffers, set the value of this
variable to '('sql-mode 'sql-batch-mode).")

(defvar sql-mark-changes t
  "*When non-nil, mark the changes that are made to an sql-results-buffer")

(defvar sql-association-mode-no-create nil
  "*Don't create a new buffer when associations are used from the
pop-up menu if non-nil.")

(defvar sql-risky-searches t
  "*Search for regexps in a risky way if non-nil.
There seems to be a bug in the function buffer-syntactic-context.  I have
not been able to reliably reproduce this bug, but setting the value of this
variable to t will suppress the hacks that prevent the bug from rearing it's
ugly head.  If set to a non-nil value, it is possible that sql code that
should be warned against will not.  This becomes a large bug as users get
more comfortable with their updates, relying on the warnings.")

(defvar sql-require-where t
  "*Require a `where' clause when the user enters SQL code matching regexp
sql-require-where-regexp.")

(defvar sql-require-where-regexp "\\<\\(delete\\|update\\|DELETE\\|UPDATE\\)[\t\n ]"
  "Regular expression matching SQL code that requires a `where' clause.")

(defvar sql-confirm-changes t
  "*Promt for a confirmation when the user enters SQL code matching regexp
sql-confirm-changes-regexp.")

(defvar sql-confirm-changes-regexp "\\<\\(delete\\|update\\|insert\\|DELETE\\|UPDATE\\|INSERT\\)[\t\n ]"
  "Regular expression matching SQL code that will make potentially
harmful changes to the database.")

(defvar sql-table-prefix-regexp "\\<\\(from\\|delete\\|update\\|into\\|truncate\\|FROM\\|DELETE\\|UPDATE\\|INTO\\|TRUNCATE\\)[\t\n ]"
  "Regular expression matching SQL code that will preceed a table name.")

(defvar sql-history-length 40
  "*Number of SQL batch commands to save in the history table.")

(defvar sql-global-history-length 40
  "*Number of SQL batch commands to save in the global history table.")
  
(defvar sql-deactivate-region nil
  "*Deactivate the region after a comment-region function is called when
non-nil.")

(defvar sql-indent-after-newline nil
  "*Indent to the proper indentation after a newline if non-nil.")

(defvar sql-inhibit-startup-message nil
  "*Non-nil causes sql-mode not to display its copyright notice, disclaimers
etc. when started in the usual way.")

(defvar sql-add-to-menu-bar t
  "*Non-nil causes sql-mode to add entries to the menu bar to facilitate
the use of associations.")

(defvar sql-parent-menu (if (and sql-xemacs (> emacs-minor-version 11))
			    '("Apps")
			  '("Utilities"))
  "The menu item under which to add SQL menus.")

(defvar sql-hungry-delete-key nil
  "*Non-nil causes delete to eat all preceeding whitespace.")

(defvar sql-delete-function 'backward-delete-char
  "*Function to delete characters backwards.")

(defvar sql-results-buffer-percent 70
  "*Percent of the window that the results buffer should take up.")

(defvar sql-greedy-results-buffers nil
  "*If non-nil, resize the results buffer to take up as much room as possible
while still leaving the sql-batch-mode buffer visible, with minimum size to
be determined by `sql-results-buffer-percent'")

(defvar sql-modeline-format '("" modeline-modified "SQL: " sql-server "  "
			      sql-user
			      sql-database-name "  %[("
			      mode-name modeline-process minor-mode-alist
			      "%n"  ")%]----" (-3 . "%p") "---"
			      sql-history-index-string "-%-")
  "Template for displaying mode line for sql-batch-mode,
sql-interactive-mode, and sql-results-mode buffers.")

(defvar sql-intersperse-headers nil
  "*Insert a header in the results buffer every N lines, where N is the
height of the results buffer.")

(defvar sql-sybase nil
  "Set the value of the SYBASE environment variable to this value if non-nil.
This provides a way to override the environment variable value.

Do not set this variable directly, instead use the function
`sql-set-sybase'.")

(defvar sql-bypass-cpp nil
  "*Bypass the cpp step when loading stored procedures when non-nil.")

(defvar sql-default-cpp-switches nil
  "*Default switches to pass to cpp when loading stored procedures.")

(defvar sql-holdup-stored-procedure nil
  "*Non-nil means keep stored procedure in batch buffer after load, and
do not execute.  Nil means execute immediately after insertion into the
buffer.")

(defvar sql-use-toolbar sql-xemacs-19-12
  "*Display a SQL Mode toolbar if non-nil.")

(defvar sql-use-toolbar nil
  "*Display a top ten toolbar if non-nil.")

(defvar sql-use-big-menus t
  "*Use larger menus if non-nil")

(defvar sql-error-regexp "Msg .* Level .* State .*\nServer .* Line "
  "*Regular expression to match error lines.")

(defvar sql-noisy t
  "*Make sounds in certain situations if non-nil.")

(defvar sql-string-column-types
  '("39" "45" "47" "58" "61")
  "*A list of column types that requre quotes.")

(defvar sql-ignore-column-types
  '("34" "35" "37" "111")
  "*A list of column types that should be ignored when doing an update.")

(defvar sql-stay-in-batch-buffer nil
  "*A non-nil value will keep the cursor in the sql-batch-mode buffer.
If the value of this variable is nil, the cursor will be in the
results buffer after executing sql-evaluate-buffer.")

;(defvar sql-reposition-windows-when-done nil
;  "*Reposition the batch buffer when the query terminates if non-nil.
;This variable only applies if you invoke the function
;`sql-evaluate-buffer-background'.")

(defvar sql-finished-query-options '(message)
  "*A list of symbols representing things to do when a query is done.
Possibilities are:

'ding     Signal with an audible bell
'open     Open (deiconify) the frame containing the batch and results buffers
'raise    Raise the frame containing the batch and results buffers
'message  Print the number of rows affected as a message in the echo area")

(defvar sql-finished-async-query-options '(message raise open)
  "*A list of symbols representing things to do when an background query is done.
Possibilities are:

'ding     Signal with an audible bell
'open     Open (deiconify) the frame containing the batch and results buffers
'raise    Raise the frame containing the batch and results buffers
'message  Print the number of rows affected as a message in the echo area")

(defvar sql-print-command (if (boundp 'enscript-switches)
			      'enscript-buffer
			    'lpr-buffer)
  "*Command to print a buffer.")

(defvar sql-print-switches (if (boundp 'enscript-switches)
			       '("-fCourier8")
			     nil)
  "*Switches to pass to sql-print-command when printing a buffer.
If non-nil, these will override the values of lpr-swithces and
enscript-switches as appropriate.")

(defvar sql-print-characters-per-line (if (boundp 'enscript-switches)
					  113
					80)
  "*Number of characters to print per line when printing results buffers.")

(defvar sql-history-file-name
  (concat (getenv "HOME") "/.sql-history-dir/sql-history")
  "Base file name in which to load and save the history information.")

(defvar sql-global-history-file-name
  (concat (getenv "HOME") "/.sql-history-dir/sql-global-history")
  "File name in which to load and save the global history information.")

(defvar sql-top-ten-file-name
  (concat (getenv "HOME") "/.sql-top-ten")
  "File name in which to load and save the top ten information.")

(defvar sql-cache-data-file-name
  (concat (getenv "HOME") "/.sql-cache-dir/sql-cache")
  "Base file name in which to load save cached information.
The server and database are appended to this base name to get the full name.")

(defvar sql-save-cache-on-exit t
  "Non-nil causes SQL Mode to automatically save the cache information
when you exit Emacs or kill the sql-batch-mode buffer.")

(defvar sql-always-load-cache t
  "Non-nil causes SQL Mode to automatically load the cache information if
it exists when you enter a sql-batch-mode buffer.")

(defvar sql-save-history-on-exit nil
  "Non-nil causes SQL Mode to automatically save the history
when you exit Emacs or kill the sql-batch-mode buffer.")

(defvar sql-always-load-history nil
  "Non-nil causes SQL Mode to automatically load the history if
it exists when you enter a sql-batch-mode buffer.")

;(defvar sql-column-separator "\|"
(defvar sql-column-separator nil
  "DO NOT USE THIS VARIABLE.  IT IS NOT FULLY IMPLEMENTED YET.

One character to use as a column separator in results buffers.
If nil, don't use any separators.

Many functions in SQL Mode rely on the alignment of columns and data.  By
setting this variable to a non-nil value, it will eliminate the errors
encountered with mal-aligned data.  In addition, some features will not
work at all unless this variable is non-nil.

Setting this variable will, however, cause a small performance penalty as
font-lock is required to make the characters invisible.

The only values that will work consistently are nil or the string \"\\|\"")

(defvar sql-change-frame-icon nil
  "Alter the icon of SQL Mode frames if non-nil.

This feature is not yet implemented.")

(defvar sql-use-interfaces-file t
  "Load the interfaces file for server completion if non-nil.")

(defvar sql-log-in-on-start nil
  "Verify that the dataserver, user, and password are correct by
logging in to the dataserver when sql-batch-mode is invoked.")

(defvar sql-stay-logged-in nil
  "*Keep an open connection to the dataserver if non-nil.")

(defvar sql-get-tables-command
  "select name from sysobjects where type in (\"U\", \"S\") order by name\ngo\n"
  "Command that will return the list of tables in the current database.")

(defvar sql-get-columns-command-prefix
  "select name, type from syscolumns where id = object_id(\""
  "Command that will return the list of columns in the current database.")

(defvar sql-get-columns-command-suffix
  "\")\ngo\n"
  "Command that will return the list of tables in the current database.")

(defvar sql-get-values-command-prefix
  "set rowcount 1000\nselect distinct "
  "Command that will return the list of values in the current database.
The \"set rowcount\" string limits the number of values that are resturned
from the dataserver when doing value-based completion.  If you remove this
part of the string, there will be no limit to the number of rows returned.")

(defvar sql-get-values-command-middle
  " from "
  "Command that will return the list of values in the current database.")

(defvar sql-get-values-command-suffix
  "\ngo\n"
  "Command that will return the list of values in the current database.")

(defvar sql-get-stored-procedures-command
  "select name from sysobjects where type = \"P\" order by name\ngo\n"
  "Command that will return the list of stored procedures in the current database.")

(defvar sql-get-databases-command
  "sp_helpdb\ngo\n"
  "Command that will return the list of databases in the server.")

(defvar sql-get-users-command
  "select name from sysusers\ngo\n"
  "Command that will return the list of userss in the server.")

(defconst sql-word-regexp "[-_.*$a-zA-Z0-9#]+" 
  "Regexp matching a word in a buffer.")

(defconst sql-line-regexp "[ \t]*\\(.*[-_.*$a-zA-Z0-9#]\\)"
  "Regexp matching a line in a buffer.")

(defvar sql-warning-prefix
  "WARNING:\n\n"
  "Prefix for warning messages.")

(defvar sql-warning-suffix
  "\n\nDo you wish to commit?\n\n(See the help on the variable `sql-confirm-changes' to supress this warning)"
  "Suffix for warning messages.")

(defvar sql-first-prompt-regexp
  "1> "
  "Regular expression matching the first prompt from the database
interaction program (i.e. isql).")

(defvar sql-all-prompts-regexp
  "[0-9]+> "
  "Regular expression matching any prompt from the database
interaction program (i.e. isql).")

(defvar sql-history-in-modeline t
  "*If non-nil display the history element number in the modeline.")

(defvar sql-use-magic-go nil
  "*Send the contents of a sql-batch-mode buffer when the user types `go'
if non-nil")

(defvar sql-preferred-operator "= "
  "Set this to a string if you would like sql-magic-yank-under-point to
have a default operator.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; End User Definable Variables.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar sql-mode-version "0.922 (beta)")

(defvar sql-mode-help-address "sql-mode-help@atlantic2.sbi.com")

(defvar sql-initialized nil)

(defvar sql-server nil)

(defvar sql-user nil)

(defvar sql-password nil)

(defvar sql-database nil)

(defvar sql-database-name nil)

(defvar sql-frame nil)

(defvar sql-batch-frame nil)

(defvar sql-results-frame nil)

(defvar sql-interactive-frame nil)

(defvar sql-mode-association nil)

(defvar sql-process nil)

(defvar sql-logged-in nil)

(defvar sql-dont-warn nil)

(defvar sql-asynchronous-flag nil)

(defvar sql-results-view-mode nil)

(defvar sql-results-edit-mode nil)

(defvar sql-completion-saved-buffer nil)

(defvar sql-edit-row-buffer nil)

(defvar sql-update-virgin-lines nil)

(defvar sql-old-point nil)

(defvar sql-temp-string nil)

(defvar sql-linked "")

(defvar sql-end-history "")

(defvar sql-completion-saved-table nil)

(defvar sql-completion-saved-column nil)

(defvar sql-query-in-progress nil)

(defvar sql-update-virgin-column-pairs nil)

(defvar sql-update-virgin-line nil)

(defvar sql-results-mode-editing nil)

(defvar sql-marking-changes nil)

(defvar sql-global-move-headers t)

(defvar sql-move-headers t)

(defvar sql-header-text nil)

(defvar sql-interfaces-file-name nil)

(defvar sql-matching-buffer nil)

(defvar sql-evaluation-buffer nil)

(defvar sql-getting-completions nil)

(defvar sql-go-count 0)

(defvar sql-association-alist nil)

(defvar sql-history nil)

(defvar sql-history-index 0)

(defvar sql-history-index-string "")

(defvar sql-global-history nil)

(defvar sql-global-history-index 0)

(defvar sql-old-history nil)

(defvar sql-old-history-index 0)

(defvar sql-top-ten (make-vector 10 nil))

(defvar sql-top-ten-aliases (make-vector 10 nil))

(defvar sql-top-ten-points (make-vector 10 nil))

(defvar sql-top-ten-execute (make-vector 10 nil))

(defvar sql-top-ten-help (make-vector 10 nil))

(defvar sql-commented-line-regexp (concat sql-comment-start-regexp
					   ".*"
					   sql-comment-end-regexp))

(defvar old-after-change-function nil)

(defvar sql-original-frame nil)

(defvar sql-current-error-point nil)

(defvar sql-old-contents nil)

(defvar sql-old-window-configuration nil)

(defvar sql-linked-windows nil)

(defvar sql-server-table nil)

(defvar sql-user-table nil)

(defvar sql-table-list nil)

(defvar sql-database-list nil)

(defvar sql-column-list nil)

(defvar sql-value-list nil)

(defvar sql-stored-procedure-list nil)

(defvar sql-user-list nil)

(defvar sql-modified-cache nil)

(defvar sql-modified-history nil)

(defvar sql-last-table nil)

(defvar sql-clipboard-table nil)

(defvar sql-clipboard-rows nil)

(defvar sql-operator-list nil)

(defvar sql-operators '("=" "like" "in" "<" ">" "!=" "<=" ">="))

(defvar sql-operator-regexp "\\(=\\|like\\|in\\|<\\|>\\|!=\\|<=\\|>=\\)")

(defvar sql-keyword-list nil)

(defvar sql-keywords '("to" "select" "from" "where" "tran" "transaction" "commit" "group" "exec" "execute" "readtext" "rollback" "compute" "union" "by" "order" "having" "set" "update" "delete" "insert" "into" "writetext" "values" "go" "use" "null" "begin" "end" "else" "if" "goto" "break" "continue" "raiserror" "waitfor" "and" "or" "not" "in" "is" "declare" "print" "return" "exists" "like" "sum" "avg" "count" "max" "min" "all" "distinct" "alter" "table" "database" "create" "disk" "nonclustered" "reconfigure" "revoke" "override" "procedure" "proc" "checkpoint" "dump" "drop" "index" "fillfactor" "rule" "shutdown" "tape" "view" "truncate" "kill" "load" "clustered" "dbcc" "grant" "as" "with" "nowait" "no_log" "refit" "reinit" "init" "mirror" "unmirror" "remirror" "default" "statistics" "TO" "SELECT" "FROM" "WHERE" "TRAN" "TRANSACTION" "COMMIT" "GROUP" "EXEC" "EXECUTE" "READTEXT" "ROLLBACK" "COMPUTE" "UNION" "BY" "ORDER" "HAVING" "SET" "UPDATE" "DELETE" "INSERT" "INTO" "WRITETEXT" "VALUES" "GO" "USE" "NULL" "BEGIN" "END" "ELSE" "IF" "GOTO" "BREAK" "CONTINUE" "RAISERROR" "WAITFOR" "AND" "OR" "NOT" "IN" "IS" "DECLARE" "PRINT" "RETURN" "EXISTS" "LIKE" "SUM" "AVG" "COUNT" "MAX" "MIN" "ALL" "DISTINCT" "ALTER" "TABLE" "DATABASE" "CREATE" "DISK" "NONCLUSTERED" "RECONFIGURE" "REVOKE" "OVERRIDE" "PROCEDURE" "PROC" "CHECKPOINT" "DUMP" "DROP" "INDEX" "FILLFACTOR" "RULE" "SHUTDOWN" "TAPE" "VIEW" "TRUNCATE" "KILL" "LOAD" "CLUSTERED" "DBCC" "GRANT" "AS" "WITH" "NOWAIT" "NO_LOG" "REFIT" "REINIT" "INIT" "MIRROR" "UNMIRROR" "REMIRROR" "DEFAULT" "STATISTICS"))

(defvar sql-keyword-regexp "\\(to\\|select\\|from\\|where\\|tran\\|transaction\\|commit\\|group\\|exec\\|execute\\|readtext\\|rollback\\|compute\\|union\\|by\\|order\\|having\\|set\\|update\\|delete\\|insert\\|into\\|writetext\\|values\\|go\\|use\\|begin\\|end\\|else\\|if\\|goto\\|break\\|continue\\|raiserror\\|waitfor\\|and\\|or\\|not\\|in\\|is\\|declare\\|print\\|return\\|exists\\|like\\|sum\\|avg\\|count\\|max\\|min\\|all\\|distinct\\|alter\\|table\\|database\\|create\\|disk\\|nonclustered\\|reconfigure\\|revoke\\|override\\|procedure\\|proc\\|checkpoint\\|dump\\|drop\\|index\\|fillfactor\\|rule\\|shutdown\\|tape\\|view\\|truncate\\|kill\\|load\\|clustered\\|dbcc\\|grant\\|as\\|with\\|nowait\\|no_log\\|refit\\|reinit\\|init\\|mirror\\|unmirror\\|remirror\\|default\\|sp_[a-zA-Z]*\\|statistics\\TO|\\|SELECT\\|FROM\\|WHERE\\|TRAN\\|TRANSACTION\\|COMMIT\\|GROUP\\|EXEC\\|EXECUTE\\|READTEXT\\|ROLLBACK\\|COMPUTE\\|UNION\\|BY\\|ORDER\\|HAVING\\|SET\\|UPDATE\\|DELETE\\|INSERT\\|INTO\\|WRITETEXT\\|VALUES\\|GO\\|USE\\|BEGIN\\|END\\|ELSE\\|IF\\|GOTO\\|BREAK\\|CONTINUE\\|RAISERROR\\|WAITFOR\\|AND\\|OR\\|NOT\\|IN\\|IS\\|DECLARE\\|PRINT\\|RETURN\\|EXISTS\\|LIKE\\|SUM\\|AVG\\|COUNT\\|MAX\\|MIN\\|ALL\\|DISTINCT\\|ALTER\\|TABLE\\|DATABASE\\|CREATE\\|DISK\\|NONCLUSTERED\\|RECONFIGURE\\|REVOKE\\|OVERRIDE\\|PROCEDURE\\|PROC\\|CHECKPOINT\\|DUMP\\|DROP\\|INDEX\\|FILLFACTOR\\|RULE\\|SHUTDOWN\\|TAPE\\|VIEW\\|TRUNCATE\\|KILL\\|LOAD\\|CLUSTERED\\|DBCC\\|GRANT\\|AS\\|WITH\\|NOWAIT\\|NO_LOG\\|REFIT\\|REINIT\\|INIT\\|MIRROR\\|UNMIRROR\\|REMIRROR\\|DEFAULT\\|SP_[a-zA-Z]*\\|STATISTICS\\)[\t\n ]")

(defvar sql-keyword-no-conjunctions-regexp "\\(to\\|select\\|from\\|where\\|tran\\|transaction\\|commit\\|group\\|exec\\|execute\\|readtext\\|rollback\\|compute\\|union\\|by\\|order\\|having\\|set\\|update\\|delete\\|insert\\|into\\|writetext\\|values\\|go\\|use\\|null\\|begin\\|end\\|else\\|if\\|goto\\|break\\|continue\\|raiserror\\|waitfor\\|not\\|in\\|is\\|declare\\|print\\|return\\|exists\\|like\\|sum\\|avg\\|count\\|max\\|min\\|all\\|distinct\\|alter\\|table\\|database\\|create\\|disk\\|nonclustered\\|reconfigure\\|revoke\\|override\\|procedure\\|proc\\|checkpoint\\|dump\\|drop\\|index\\|fillfactor\\|rule\\|shutdown\\|tape\\|view\\|truncate\\|kill\\|load\\|clustered\\|dbcc\\|grant\\|as\\|with\\|nowait\\|no_log\\|refit\\|reinit\\|init\\|mirror\\|unmirror\\|remirror\\|default\\|sp_[a-zA-Z]*\\|statistics\\|TO\\|SELECT\\|FROM\\|WHERE\\|TRAN\\|TRANSACTION\\|COMMIT\\|GROUP\\|EXEC\\|EXECUTE\\|READTEXT\\|ROLLBACK\\|COMPUTE\\|UNION\\|BY\\|ORDER\\|HAVING\\|SET\\|UPDATE\\|DELETE\\|INSERT\\|INTO\\|WRITETEXT\\|VALUES\\|GO\\|USE\\|NULL\\|BEGIN\\|END\\|ELSE\\|IF\\|GOTO\\|BREAK\\|CONTINUE\\|RAISERROR\\|WAITFOR\\|NOT\\|IN\\|IS\\|DECLARE\\|PRINT\\|RETURN\\|EXISTS\\|LIKE\\|SUM\\|AVG\\|COUNT\\|MAX\\|MIN\\|ALL\\|DISTINCT\\|ALTER\\|TABLE\\|DATABASE\\|CREATE\\|DISK\\|NONCLUSTERED\\|RECONFIGURE\\|REVOKE\\|OVERRIDE\\|PROCEDURE\\|PROC\\|CHECKPOINT\\|DUMP\\|DROP\\|INDEX\\|FILLFACTOR\\|RULE\\|SHUTDOWN\\|TAPE\\|VIEW\\|TRUNCATE\\|KILL\\|LOAD\\|CLUSTERED\\|DBCC\\|GRANT\\|AS\\|WITH\\|NOWAIT\\|NO_LOG\\|REFIT\\|REINIT\\|INIT\\|MIRROR\\|UNMIRROR\\|REMIRROR\\|DEFAULT\\|SP_[a-zA-Z]*\\|STATISTICS\\)[\t\n ]")

(defvar sql-bos-regexp "\\(end\\|return\\|go\\|if\\|else\\|begin\\|select\\|update\\|END\\|RETURN\\|GO\\|IF\\|ELSE\\|BEGIN\\|SELECT\\|UPDATE\\)\\>")

(defvar sql-startup-message-lines
  '("Type C-c h for general help on SQL Mode."
    "Type C-h m for help on the current mode."
    "Please use \\[sql-submit-bug-report] to report bugs."
    "Please use \\[sql-submit-enhancement-request] to request enhancements."
    "This is prerelease software.  Use at your own risk."
    "SQL Mode comes with ABSOLUTELY NO WARRANTY."
    "Closed Captioned (CC) for the hearing impaired."))

(defvar sql-command-level 1)

(defconst sql-options-menu-saved-forms
  (purecopy
   '(sql-font-lock-buffers
     sql-mark-changes
     sql-video-type
     sql-command
     sql-batch-command-switches
     sql-interactive-command-switches
     sql-history-length
     sql-global-history-length
     sql-require-final-go
     sql-secure-passwords
     sql-abbrev-mode
;     sql-minibuffer-status
     sql-comment-regions-by-line
     sql-require-where
     sql-confirm-changes
     sql-indent-after-newline
     sql-hungry-delete-key
     sql-save-all-results
     sql-results-in-new-frame
     sql-resize-results-frames
     sql-results-frame-width
     sql-results-frame-height
     sql-max-frame-width
     sql-intersperse-headers
     sql-use-toolbar
     sql-use-top-ten-toolbar
     sql-use-big-menus
     sql-stay-in-batch-buffer
     sql-always-load-cache
     sql-save-cache-on-exit
     sql-always-load-history
     sql-save-history-on-exit
     sql-log-in-on-start
     sql-evaluation-method
     sql-stay-logged-in
     sql-use-magic-go))
  "The variables to save; or forms to evaluate to get forms to write out.")

(defvar sql-old-menu (and sql-xemacs (copy-sequence current-menubar)))

(defvar sql-execute-menu
  '(("Execute"
     ["Evaluate Buffer"			sql-evaluate-buffer		t]
;     ["Evaluate Buffer in Background"	sql-evaluate-buffer-background	t]
     ["Evaluate Region"			sql-evaluate-region		(mark)]
     "----"
     ["Abort Evaluation"		sql-abort			t]
     ["Close Database Connection"	sql-log-out		sql-logged-in]
     "----"
     ["Run sp_lock"			sql-sp-lock			t]
     ["Run sp_who"			sql-sp-who			t]
     ["Run sp_what"			sql-sp-what			t]
     "----"
     ("Execute Top Ten Item"
      ["Execute Top Ten #1"		sql-run-top-ten-1
      (aref sql-top-ten 1)]
      ["Execute Top Ten #2"		sql-run-top-ten-2
      (aref sql-top-ten 2)]
      ["Execute Top Ten #3"		sql-run-top-ten-3
      (aref sql-top-ten 3)]
      ["Execute Top Ten #4"		sql-run-top-ten-4
      (aref sql-top-ten 4)]
      ["Execute Top Ten #5"		sql-run-top-ten-5
      (aref sql-top-ten 5)]
      ["Execute Top Ten #6"		sql-run-top-ten-6
      (aref sql-top-ten 6)]
      ["Execute Top Ten #7"		sql-run-top-ten-7
      (aref sql-top-ten 7)]
      ["Execute Top Ten #8"		sql-run-top-ten-8
      (aref sql-top-ten 8)]
      ["Execute Top Ten #9"		sql-run-top-ten-9
      (aref sql-top-ten 9)]
      ["Execute Top Ten #0          "	sql-run-top-ten-0
      (aref sql-top-ten 0)])
     "----"
     ["Load Stored Procedure (CPP)"   	sql-load-sp			t]
     ["Load Stored Procedure (No CPP)"  (let ((sql-bypass-cpp t))
					  (call-interactively 'sql-load-sp)) t]
     "----"
     ["Cut Data"			sql-cut-data			t]
     ["Copy Data"			sql-copy-data			t]
     ["Paste Data"			sql-paste-data			t]
     ["Delete Data"			sql-delete-data			t]
     ["Edit Data"			sql-edit-row			t]
     ["Insert Row..."			sql-insert-row			t]
     "----"
     ["BCP Table IN..."			sql-bcp-in-menu			t]
     ["BCP Table OUT..."		sql-bcp-out-menu		t]
     ["Fake BCP OUT..."			sql-fake-bcp-out		t]
     )))
     
(defvar sql-actions-menu
  '(("Actions"
     ["Insert File..."			sql-insert-file			t]
     "----"
     ["Insert Stored Procedure (CPP)"	sql-insert-sp			t]
     ["Insert Stored Procedure (No CPP)"	(sql-insert-sp t)	t]
     "----"
     ("Insert Top Ten Item"
      ["Insert Top Ten #1"		sql-insert-top-ten-1
      (aref sql-top-ten 1)]
      ["Insert Top Ten #2"		sql-insert-top-ten-2
      (aref sql-top-ten 2)]
      ["Insert Top Ten #3"		sql-insert-top-ten-3
      (aref sql-top-ten 3)]
      ["Insert Top Ten #4"		sql-insert-top-ten-4
      (aref sql-top-ten 4)]
      ["Insert Top Ten #5"		sql-insert-top-ten-5
      (aref sql-top-ten 5)]
      ["Insert Top Ten #6"		sql-insert-top-ten-6
      (aref sql-top-ten 6)]
      ["Insert Top Ten #7"		sql-insert-top-ten-7
      (aref sql-top-ten 7)]
      ["Insert Top Ten #8"		sql-insert-top-ten-8
      (aref sql-top-ten 8)]
      ["Insert Top Ten #9"		sql-insert-top-ten-9
      (aref sql-top-ten 9)]
      ["Insert Top Ten #0          "	sql-insert-top-ten-0
      (aref sql-top-ten 0)])
     ["Save Current as Top Ten"		sql-add-top-ten			t]
     "----"
     ["Erase Batch and Results Buffers" sql-new-query			t]
     ["Clear Cached Data"		sql-clear-cached-data		t]
     ("Clear Specific Cache"
      ["Clear Table List"		sql-clear-cached-table-data     t]
      ["Clear Column List"		sql-clear-cached-column-data    t]
      ["Clear Stored Procedure List"
       sql-clear-cached-stored-procedure-data     t]
      ["Clear Datablase List"      	sql-clear-cached-database-data  t]
      ["Clear User List"      		sql-clear-cached-user-data  t])
     ["Load Cached Data"		sql-load-cache-data		t]
     ["Save Cached Data"		sql-save-cache-data
      sql-modified-cache]
     "----"
     ["Recenter"			sql-recenter			t]
     ["Reposistion Windows"		sql-reposition-windows		t]
     ["Pop Results Buffer"		sql-pop-and-rename-buffer 	t]
     "----"
     ["Next Error"			sql-next-error			t]
     ["Previous Error"			sql-previous-error		t]
     "----"
     ["Print Buffer"			sql-print-buffer		t]
     "----"
     ["Exit SQL Mode"			sql-exit-sql-mode		t]
     )))

(defvar sql-history-menu
  '(("History"
     ["Previous History"		sql-previous-history		t]
     ["Next History"			sql-next-history		t]
     ["Previous History Unlinked"	(sql-previous-history t)	t]
     ["Next History Unlinked"		(sql-next-history t)		t]
     ["Previous Global History"		sql-previous-global-history	t]
     ["Next Global History"		sql-next-global-history		t]
     ["Previous Matching History"	sql-previous-matching-history	t]
     ["Goto History..."			sql-goto-history		t]     
     ["Goto Global History..."		sql-goto-global-history		t]
     "----"
     ["Save History"			sql-save-history		sql-modified-history]
     ["Load History"			sql-load-history		t]
     ["Save Global History"		sql-save-global-history		t]
     ["Load Global History"		sql-load-global-history		t]
     )))

(defvar sql-settings-menu
  '(("Settings"
     ["Use Database..."			sql-set-database		t]
     ["Reset Database"			sql-reset-database		t]
     "----"
     ["Set Server, User, and Password..."	sql-set-server		t]
     ["Set User and Password..."	sql-set-user			t]
     ["Set Password..."			sql-set-password		t]
     "----"
     ["Set BCP User..."			sql-set-bcp-user		t]
     )))

(defvar sql-help-menu
  '(("Help"
     ["About SQL Mode..."		sql-about-sql-mode		t]
     "----"
     ["SQL Info (Detailed Docs)"	sql-info			nil]
     ["SQL News"			sql-show-news			t]
     ["Current Buffer Info"		sql-current-buffer-info		t]
     "----"
     ["Describe Mode"			describe-mode			t]
     ["Describe Key..."			describe-key			t]
     ["Describe Function..."		describe-function		t]
     ["Describe Variable..."		describe-variable		t]
     "----"
     ["Request Latest Version"		sql-request-latest-version	t]
     ["Submit Enhancement Request"	sql-submit-enhancement-request	t]
     ["Submit Bug Report"		sql-submit-bug-report		t]
     )))

(defvar sql-font-lock-menu
  '(("Coloring"
     ["None"	(setq sql-font-lock-buffers nil)
      :style radio
      :selected (eq sql-font-lock-buffers nil)]
     ["All"	(setq sql-font-lock-buffers 'all)
      :style radio
      :selected (eq sql-font-lock-buffers 'all)]
     "----"
     ["SQL Mode Buffers"  		(sql-toggle-font-lock 'sql-mode)
      :style toggle
      :selected (or (eq sql-font-lock-buffers 'all)
		    (member 'sql-mode sql-font-lock-buffers))]
     ["SQL Batch Mode Buffers"  	(sql-toggle-font-lock 'sql-batch-mode)
      :style toggle
      :selected (or (eq sql-font-lock-buffers 'all)
		    (member 'sql-batch-mode sql-font-lock-buffers))]
     ["SQL Interactive Buffers"  	(sql-toggle-font-lock
					 'sql-interactive-mode)
      :style toggle
      :selected (or (eq sql-font-lock-buffers 'all)
		    (member 'sql-interactive-mode sql-font-lock-buffers))]
     ["SQL Results Buffers"  		(sql-toggle-font-lock
					 'sql-results-mode)
      :style toggle
      :selected (or (eq sql-font-lock-buffers 'all)
		    (member 'sql-results-mode sql-font-lock-buffers))]
     "----"
     ["Mark Changes" 			(sql-toggle-marking-changes)
      :style toggle
      :selected sql-marking-changes]
     "----"
     ["Regular Video"			(progn (setq sql-video-type 'regular)
					       (sql-setup-font-lock))
      :style radio
      :selected (eq sql-video-type 'regular)]
     ["Inverse Video"			(progn (setq sql-video-type 'inverse)
					       (sql-setup-font-lock))
      :style radio
      :selected (eq sql-video-type 'inverse)]
     ["Monochrome"			(progn (setq sql-video-type 'monochrome)
					       (sql-setup-font-lock))
      :style radio
      :selected (eq sql-video-type 'monochrome)]
     "----"
     ["Current Buffer"		sql-toggle-current-font-lock
      :style toggle
      :selected font-lock-mode])))

(defvar sql-options-menu
  (list
   (append
    '("Options")
    sql-font-lock-menu
    '(["SYBASE Environment Variable..."	sql-set-sybase			 t]
      ["SQL Command..."		sql-set-sql-command			 t]
      ["SQL Batch Command Switches..."
       (sql-set-variable 'sql-batch-command-switches) 			 t]
      ["SQL Interactive Command Switches..."
       (sql-set-variable 'sql-interactive-command-switches) 		 t]
      ["History Length..."	(sql-set-variable 'sql-history-length t) t]
      ["Global History Length..." (sql-set-variable 'sql-global-history-length
						    t) t]
      "----"
      ("Evaluation Method"
       ["Synchronous"	(setq sql-evaluation-method 'foreground)	 
	:style radio
	:selected (eq sql-evaluation-method 'foreground)]
       ["Asynchronous"	(setq sql-evaluation-method 'background)
	:style radio
	:selected (eq sql-evaluation-method 'background)])
      ("Dataserver Connection"
       ["Stay Logged In" (sql-set-stay-logged-in t)
       :style radio
       :selected sql-stay-logged-in]
       ["Don't Stay Logged In" (sql-set-stay-logged-in nil)
       :style radio
       :selected (not sql-stay-logged-in)]
       "----"
       ["Verify Log In On Start-Up" (setq sql-log-in-on-start
				   (not sql-log-in-on-start))
	:style toggle
	:selected sql-log-in-on-start])
      "----"
      ["Require Final `go'"		(setq sql-require-final-go
					      (not sql-require-final-go))
       :style toggle
       :selected sql-require-final-go]
      ["Use Magic `go'"		(setq sql-use-magic-go
					      (not sql-use-magic-go))
       :style toggle
       :selected sql-use-magic-go]
      ["Secure Passwords"		(setq sql-secure-passwords
					      (not sql-secure-passwords))
       :style toggle
       :selected sql-secure-passwords]
      ["Abbrev Mode"		(progn (setq sql-abbrev-mode
					     (not sql-abbrev-mode))
				       (call-interactively 'abbrev-mode))
       :style toggle
       :selected abbrev-mode]
;      ["Display Status in Minibuffer" 	(setq sql-minibuffer-status
;					      (not sql-minibuffer-status))
;       :style toggle
;       :selected sql-minibuffer-status]
      ["Comment Regions By Line"	(setq sql-comment-regions-by-line
					      (not sql-comment-regions-by-line))
       :style toggle
       :selected sql-comment-regions-by-line]
      ["Stay in Batch Buffer"		(setq sql-stay-in-batch-buffer
					      (not sql-stay-in-batch-buffer))
       :style toggle
       :selected sql-stay-in-batch-buffer]
      "----"
      ["Require Where Clause"		(setq sql-require-where
					      (not sql-require-where))
       :style toggle
       :selected sql-require-where]
      ["Confirm All Changes"		(setq sql-confirm-changes
					      (not sql-confirm-changes))
       :style toggle
       :selected sql-confirm-changes]
      "----"
;      ["Auto Indent"			(setq sql-indent-after-newline
;					      (not sql-indent-after-newline))
;       :style toggle
;       :selected sql-indent-after-newline]
;      ["Hungry Delete"			(setq sql-hungry-delete-key
;					      (not sql-hungry-delete-key))
;       :style toggle
;       :selected sql-hungry-delete-key]
;      "----"
;      ["Save All Results"		(setq sql-save-all-results
;					      (not sql-save-all-results))
;       :style toggle
;       :selected sql-save-all-results]
;      ["Resize Results Frames"		(setq sql-resize-results-frames
;					      (not sql-resize-results-frames))
;       :style toggle
;       :selected sql-resize-results-frames]
;      ["Results Frame Width..."		(sql-set-variable
;						 'sql-results-frame-width t)
;       sql-resize-results-frames]
;      ["Results Frame Height..."	(sql-set-variable 
;					 'sql-results-frame-height t)
;       sql-resize-results-frames]
;      ["Maximum Results Width..."	(sql-set-variable
;					 'sql-max-frame-width t)
;       t]
;      ["Intersperse Headers"		(setq sql-intersperse-headers
;					      (not sql-intersperse-headers))
;       :style toggle
;       :selected sql-intersperse-headers]
      ["Always Load History"	(setq sql-always-load-history
				      (not sql-always-load-history))
       :style toggle
       :selected sql-always-load-history]
      ["Save History on Exit"	(setq sql-save-history-on-exit
					      (not sql-save-history-on-exit))
       :style toggle
       :selected sql-save-history-on-exit]
      "----"
      ["Always Load Cached Data"	(setq sql-always-load-cache
					      (not sql-always-load-cache))
       :style toggle
       :selected sql-always-load-cache]
      ["Save Cached Data on Exit"	(setq sql-save-cache-on-exit
					      (not sql-save-cache-on-exit))
       :style toggle
       :selected sql-save-cache-on-exit]
      "----"
      ("New Frame For"
       ["Batch Buffers"			(setq sql-batch-in-new-frame
					      (not sql-batch-in-new-frame))
	:style toggle
	:selected sql-batch-in-new-frame]
       ["Results Buffers"		(setq sql-results-in-new-frame
					      (not sql-results-in-new-frame))
	:style toggle
	:selected sql-results-in-new-frame]
       ["Interactive Buffers"	(setq sql-interactive-in-new-frame
				      (not sql-interactive-in-new-frame))
	:style toggle
	:selected sql-interactive-in-new-frame]
       )
      ("Frame Appearance"
       ["Display SQL Mode Toolbar"	sql-toolbar
	:style toggle
	:selected sql-use-toolbar]
       ["Display Top Ten Toolbar"	sql-top-ten-toolbar
	:style toggle
	:selected sql-use-top-ten-toolbar]
       "----"
       ["Use Big Menus"			sql-toggle-big-menus
	:style toggle
	:selected sql-use-big-menus]
       )
      "----"
      ["Save Current Options"	sql-save-current-options		 t])
					;   '("----")
    )))

(defvar sql-mode-menu
  (append
   '("SQL")
   (if (and sql-lucid (boundp 'emacs-minor-version) (> emacs-minor-version 9))
       sql-options-menu
     '(["Auto Indent"			sql-toggle-auto-indent t]
       "----"))
   '(["Set Evaluation Buffer (Association)" sql-mode-set-association	 t]
     ["Set Evaluation Buffer"		sql-mode-set-evaluation-buffer	 t]
     ["Evaluate Buffer"			sql-mode-evaluate-buffer	 t]
     ["Evaluate Region"			sql-mode-evaluate-region	 (mark)]
     ["Evaluate Statement"		sql-mode-evaluate-statement	 t]
     "----"
     ["Indent Line"			sql-indent-line			 t]
     ["Indent Region"			indent-region			 t]
     "----"
     ["Toggle Line Comments"		sql-comment-line-toggle  	 t]
     "----"
     ["Comment Region"			sql-comment-region		 t]
     ["Uncomment Region"		sql-uncomment-region		 t]
     ["Toggle Region Comments"		sql-comment-region-toggle	 t]
     "----"
     ["Comment Buffer"			sql-comment-buffer		 t]
     ["Uncomment Buffer"		sql-uncomment-buffer		 t]
     ["Toggle Buffer Comments"		sql-comment-buffer-toggle	 t]
     "----"
     ["Show Current Buffer Info"	sql-current-buffer-info	 	 t]
     "----"
     ["Submit Enhancement Request"	sql-submit-enhancement-request	 t]
     ["Submit Bug Report"		sql-submit-bug-report		 t]
     "----"
     ["About SQL Mode..."		sql-about-sql-mode		 t]
     "----"
     ["Exit SQL Mode"			sql-exit-sql-mode		 t]
     )))

(defvar sql-batch-mode-menu
  (append
   '("SQL")
    (if (and sql-lucid (boundp 'emacs-minor-version) (> emacs-minor-version 9))
	sql-options-menu
      '(["Save All Results"  		sql-toggle-save-all-results      t]
	["Results in New Frame" 	sql-toggle-results-in-new-frame t]
	"----"))
    '(["Evaluate Buffer"		sql-evaluate-buffer		 t]
;      ["Evaluate Buffer in Background"	sql-evaluate-buffer-background	 t]
      ["Evaluate Region"		sql-evaluate-region              t]
      ["Abort Evaluation"		sql-abort			 t]
      "----"
      ["Use Database..."		sql-set-database		 t]
      ["Reset Database"			sql-reset-database		 t]
      ["Clear Cached Data"		sql-clear-cached-data		 t]
      "----"
      ["Set Server, User, and Password..."	sql-set-server		 t]
      ["Set User and Password..."	sql-set-user			 t]
      ["Set Password..."		sql-set-password		 t]
      "----"
      ("Comments"
       ["Toggle Line Comments"		sql-comment-line-toggle		 t]
       "----"
       ["Comment Region"		sql-comment-region		 t]
       ["Uncomment Region"		sql-uncomment-region		 t]
       ["Toggle Region Comments"	sql-comment-region-toggle	 t]
       "----"
       ["Comment Buffer"		sql-comment-buffer		 t]
       ["Uncomment Buffer"		sql-uncomment-buffer		 t]
       ["Toggle Buffer Comments"	sql-comment-buffer-toggle	 t])
      "----"
      ["Save Current History"		sql-save-history		 t]
      ["Load History"			sql-load-history		 t]
      "----"
      ["Save Global History"		sql-save-global-history		 t]
      ["Load Global History"		sql-load-global-history		 t]
      "----"
      ["Show Current Buffer Info"	sql-current-buffer-info		 t]
      "----"
      ["Submit Enhancement Request"	sql-submit-enhancement-request	 t]
      ["Submit Bug Report"		sql-submit-bug-report		 t]
      "----"
      ["About SQL Mode..."		sql-about-sql-mode		 t]
      "----"
      ["Exit SQL Mode"			sql-exit-sql-mode		 t]
      )))

(defvar sql-interactive-mode-menu
  (append
   '("SQL")
   (if (and sql-lucid (boundp 'emacs-minor-version) (> emacs-minor-version 9))
       sql-options-menu
     ())
   '(["Toggle Line Comments"		sql-comment-line-toggle		 t]
     "----"
     ["Scroll Left"			scroll-left			 t]
     ["Scroll Right"			scroll-right			 t]
     "----"
     ["Forward Column"			sql-forward-column		 t]
     ["Backward Column"			sql-backward-column		 t]
     "----"
     ["Recenter"			sql-recenter			 t]
     "----"
     ["Show Current Buffer Info"	sql-current-buffer-info		 t]
     "----"
     ["Submit Enhancement Request"	sql-submit-enhancement-request	 t]
     ["Submit Bug Report"		sql-submit-bug-report		 t]
     "----"
     ["About SQL Mode..."		sql-about-sql-mode		 t]
     "----"
     ["Exit SQL Mode"			sql-exit-sql-mode		 t]
     )))

(defvar sql-results-mode-menu
  (append
   '("SQL")
   (if (and sql-lucid (boundp 'emacs-minor-version) (> emacs-minor-version 9))
       sql-options-menu
     ())
   '(["Edit Mode"			sql-edit-toggle
      :style toggle
      :selected sql-results-mode-editing]
     ["First Row"			sql-first-row			 t]
     ["Last Row"			sql-last-row			 t]
     ["Beginning of Row"		sql-beginning-of-row		 t]
     ["End of Row"			sql-end-of-row			 t]
     "----"
     ["Forward Column"			sql-forward-column		 t]
     ["Backward Column"			sql-backward-column		 t]
     "----"
;     ["Insert Row..."			sql-insert-row			 t]
     ["Edit Row at Point"		sql-edit-row			 t]
     ["Delete Row at Point"		sql-delete-row			 t]
     "----"
     ["Insert Header"			sql-insert-header		 t]
     ["Recenter"			sql-recenter			 t]
     "----"
     ["Pop Results Buffer"		sql-pop-and-rename-buffer 	 t]
     ["Print Results Buffer"		sql-print-buffer-tiled	 	 t]
     "----"
     ["Show Current Buffer Info"	sql-current-buffer-info		 t]
     "----"
     ["Submit Enhancement Request"	sql-submit-enhancement-request	 t]
     ["Submit Bug Report"		sql-submit-bug-report		 t]
     "----"
     ["About SQL Mode..."		sql-about-sql-mode		 t]
     "----"
     ["Exit SQL Mode"			sql-exit-sql-mode		 t]
     )))

(defvar sql-mode-syntax-table nil
  "Syntax table used while in sql-mode.")
(if sql-mode-syntax-table
    ()
  (setq sql-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?/ ". 14" sql-mode-syntax-table)  ; comment start
  (modify-syntax-entry ?* ". 23" sql-mode-syntax-table)
  (modify-syntax-entry ?+ "." sql-mode-syntax-table)
  (modify-syntax-entry ?- "." sql-mode-syntax-table)
  (modify-syntax-entry ?= "." sql-mode-syntax-table)
  (modify-syntax-entry ?% "w" sql-mode-syntax-table)
  (modify-syntax-entry ?< "." sql-mode-syntax-table)
  (modify-syntax-entry ?> "." sql-mode-syntax-table)
  (modify-syntax-entry ?& "w" sql-mode-syntax-table)
  (modify-syntax-entry ?| "." sql-mode-syntax-table)
  (modify-syntax-entry ?_ "w" sql-mode-syntax-table)    ; make _ part of words
  (modify-syntax-entry ?\' "\"" sql-mode-syntax-table))

(defvar sql-help-map
  (let ((map (make-sparse-keymap)))
    (and sql-lucid (set-keymap-name map 'sql-help-map))
    (and sql-lucid (set-keymap-prompt map "(Type h or ? for further options)"))
    map)
  "Keymap for characters following the SQL Help key.")

(fset 'sql-help-command sql-help-map)

(define-key sql-help-map "h" 'sql-help-for-help)
(define-key sql-help-map "?" 'sql-help-for-help)
(define-key sql-help-map "a" 'sql-advanced-usage-info)
(define-key sql-help-map "b" 'sql-basic-usage-info)
(define-key sql-help-map "c" 'sql-customization-info)
;(define-key sql-help-map "e" 'sql-evaluate-buffer)
(define-key sql-help-map "i" 'sql-current-buffer-info)
(define-key sql-help-map "k" 'sql-describe-keybindings)
(define-key sql-help-map "m" 'sql-masthead-info)
(define-key sql-help-map "n" 'sql-next-error)
(define-key sql-help-map "p" 'sql-previous-error)
(define-key sql-help-map "r" 'sql-bug-report-info)
(define-key sql-help-map "s" 'sql-about-sql-mode)
(define-key sql-help-map "q" 'sql-help-quit)

(defvar sql-mode-map ()
  "Keymap used while in an sql-mode buffer.")
(if sql-mode-map
    ()
  (setq sql-mode-map (make-keymap))
  (and sql-lucid (set-keymap-name sql-mode-map 'sql-mode-map))
;  (define-key sql-mode-map [(return)] 'sql-newline-maybe-indent)
;  (define-key sql-mode-map [(newline)] 'sql-newline-maybe-indent)
  (define-key sql-mode-map [(delete)] 'sql-electric-delete)
  (define-key sql-mode-map "\C-c\C-c" 'sql-comment-line-toggle)
  (define-key sql-mode-map "\C-c\C-b" 'sql-comment-buffer-toggle)
  (define-key sql-mode-map "\C-c\C-i" 'sql-comment-buffer)
  (define-key sql-mode-map "\C-c\C-r" 'sql-uncomment-buffer)
  (define-key sql-mode-map "\C-c\C-q" 'sql-exit-sql-mode)
  (define-key sql-mode-map "\C-cc" 'sql-display-completion-context)
  (define-key sql-mode-map "\C-c\C-s" 'sql-show-syntactic-information)
  (define-key sql-mode-map "\C-ch" 'sql-help-command)
  (define-key sql-mode-map "\M-\i" 'sql-mode-evaluate-buffer)
  (define-key sql-mode-map "o" 'sql-magic-go))

(defvar sql-batch-mode-map ()
  "Keymap used while in an sql-batch-mode buffer.")
(if sql-batch-mode-map
    ()
  (setq sql-batch-mode-map (make-keymap))
  (and sql-lucid (set-keymap-name sql-batch-mode-map 'sql-batch-mode-map))
  (define-key sql-batch-mode-map [(delete)] 'sql-electric-delete)
  (define-key sql-batch-mode-map "\C-c\C-e" 'sql-evaluate-buffer)
  (define-key sql-batch-mode-map "\M-\i" 'sql-evaluate-buffer)
  (define-key sql-batch-mode-map "\M-e" 'sql-evaluate-buffer-asynchronous)
  (define-key sql-batch-mode-map "\C-c\C-c" 'sql-comment-line-toggle)
  (define-key sql-batch-mode-map "\C-c\C-b" 'sql-comment-buffer-toggle)
  (define-key sql-batch-mode-map "\C-c\C-i" 'sql-comment-buffer)
  (define-key sql-batch-mode-map "\C-c\C-r" 'sql-uncomment-buffer)
  (define-key sql-batch-mode-map "\C-c\C-s" 'sql-set-server)
  (define-key sql-batch-mode-map "\C-c\C-u" 'sql-set-user)
  (define-key sql-batch-mode-map "\C-c\C-p" 'sql-set-password)
  (define-key sql-batch-mode-map "\C-c\C-a" 'sql-association-mode)
  (define-key sql-batch-mode-map "\C-c\C-d" 'sql-set-database)
  (define-key sql-batch-mode-map "\C-c\C-t" 'sql-insert-top-ten)
  (define-key sql-batch-mode-map "\C-c0" 'sql-insert-top-ten-0)
  (define-key sql-batch-mode-map "\C-c1" 'sql-insert-top-ten-1)
  (define-key sql-batch-mode-map "\C-c2" 'sql-insert-top-ten-2)
  (define-key sql-batch-mode-map "\C-c3" 'sql-insert-top-ten-3)
  (define-key sql-batch-mode-map "\C-c4" 'sql-insert-top-ten-4)
  (define-key sql-batch-mode-map "\C-c5" 'sql-insert-top-ten-5)
  (define-key sql-batch-mode-map "\C-c6" 'sql-insert-top-ten-6)
  (define-key sql-batch-mode-map "\C-c7" 'sql-insert-top-ten-7)
  (define-key sql-batch-mode-map "\C-c8" 'sql-insert-top-ten-8)
  (define-key sql-batch-mode-map "\C-c9" 'sql-insert-top-ten-9)
  (define-key sql-batch-mode-map '[(control c) (control \0)] 'sql-run-top-ten-0)
  (define-key sql-batch-mode-map '[(control c) (control \1)] 'sql-run-top-ten-1)
  (define-key sql-batch-mode-map '[(control c) (control \2)] 'sql-run-top-ten-2)
  (define-key sql-batch-mode-map '[(control c) (control \3)] 'sql-run-top-ten-3)
  (define-key sql-batch-mode-map '[(control c) (control \4)] 'sql-run-top-ten-4)
  (define-key sql-batch-mode-map '[(control c) (control \5)] 'sql-run-top-ten-5)
  (define-key sql-batch-mode-map '[(control c) (control \6)] 'sql-run-top-ten-6)
  (define-key sql-batch-mode-map '[(control c) (control \7)] 'sql-run-top-ten-7)
  (define-key sql-batch-mode-map '[(control c) (control \8)] 'sql-run-top-ten-8)
  (define-key sql-batch-mode-map '[(control c) (control \9)] 'sql-run-top-ten-9)
  (define-key sql-batch-mode-map "\C-ct" 'sql-add-top-ten)
  (define-key sql-batch-mode-map "\C-cc" 'sql-display-completion-context)
  (define-key sql-batch-mode-map "\M-p" 'sql-previous-history)
  (define-key sql-batch-mode-map "\M-n" 'sql-next-history)
  (define-key sql-batch-mode-map "\M-m" 'sql-previous-matching-history)
  (define-key sql-batch-mode-map "\M-P" 'sql-previous-global-history)
  (define-key sql-batch-mode-map "\M-N" 'sql-next-global-history)
  (define-key sql-batch-mode-map "\C-cn" 'sql-next-error)
  (define-key sql-batch-mode-map "\C-cp" 'sql-previous-error)
  (define-key sql-batch-mode-map "\C-c\C-y" 'sql-paste)
  (define-key sql-batch-mode-map [(tab)] 'sql-complete-word-maybe)
  (define-key sql-batch-mode-map [(control L)] 'sql-reposition-windows)
  (define-key sql-batch-mode-map "\C-l" 'sql-recenter)
  (define-key sql-batch-mode-map [(button3)] 'sql-dynamic-popup-menu)
  (define-key sql-batch-mode-map "\C-c\C-q" 'sql-exit-sql-mode)
  (define-key sql-batch-mode-map "\C-ch" 'sql-help-command)
  (define-key sql-batch-mode-map "\C-c\C-o" 'sql-goto-results-buffer)
  (define-key sql-batch-mode-map "\C-cl" 'sql-reposition-windows)
  (define-key sql-batch-mode-map "o" 'sql-magic-go))

(defvar sql-interactive-mode-map ()
  "Keymap used while in an sql-interactive-mode buffer.")
(if sql-interactive-mode-map
    ()
  (setq sql-interactive-mode-map (copy-keymap comint-mode-map))
  (and sql-lucid
       (set-keymap-name sql-interactive-mode-map 'sql-interactive-mode-map))
;  (define-key sql-interactive-mode-map [(delete)] 'sql-electric-delete)
  (define-key sql-interactive-mode-map "\C-a" 'sql-beginning-of-command-line)
  (define-key sql-interactive-mode-map "\C-e" 'sql-end-of-row)
  (define-key sql-interactive-mode-map [(backspace)] 'sql-backward-delete-char)
  (define-key sql-interactive-mode-map [(delete)] 'sql-backward-delete-char)
  (define-key sql-interactive-mode-map [(control F)] 'sql-forward-column)
  (define-key sql-interactive-mode-map [(control B)] 'sql-backward-column)
;  (define-key sql-interactive-mode-map "\M-b" 'scroll-right)
;  (define-key sql-interactive-mode-map "\M-f" 'scroll-left)
  (if sql-lucid
      (progn
	(define-key sql-interactive-mode-map [(control >)] 'sql-forward-column)
	(define-key sql-interactive-mode-map [(control <)] 'sql-backward-column)
	(define-key sql-interactive-mode-map [(control \.)] 'scroll-left)
	(define-key sql-interactive-mode-map [(control ,)] 'scroll-right))
    (define-key sql-interactive-mode-map (quote [4194364]) 'sql-backward-column)
    (define-key sql-interactive-mode-map (quote [4194366]) 'sql-forward-column)
    (define-key sql-interactive-mode-map (quote [4194348]) 'scroll-right)
    (define-key sql-interactive-mode-map (quote [4194350]) 'scroll-left))
  (define-key sql-interactive-mode-map "\C-L" 'sql-recenter)
  (define-key sql-interactive-mode-map [(shift right)] 'sql-forward-column)
  (define-key sql-interactive-mode-map [(shift left)] 'sql-backward-column)
  (define-key sql-interactive-mode-map [(shift button1)] 'sql-drag-display)
  (define-key sql-interactive-mode-map [(button3)] 'sql-popup-association-menu)
  (define-key sql-interactive-mode-map "\C-c\C-q" 'sql-exit-sql-mode)
  (define-key sql-interactive-mode-map "\C-ch" 'sql-help-command)
  (define-key sql-interactive-mode-map [(tab)] 'sql-complete-word-maybe))

(defvar sql-results-mode-map ()
  "Keymap used while in an sql-results-mode buffer.")
(if sql-results-mode-map
    ()
  (setq sql-results-mode-map (make-keymap))
  (and sql-lucid (set-keymap-name sql-results-mode-map 'sql-results-mode-map))
  (suppress-keymap sql-results-mode-map)
  (define-key sql-results-mode-map "\M-<" 'sql-beginning-of-buffer)
  (define-key sql-results-mode-map "\M->" 'sql-end-of-buffer)
  (define-key sql-results-mode-map "<" 'sql-backward-column)
  (define-key sql-results-mode-map "," 'scroll-right)
  (define-key sql-results-mode-map ">" 'sql-forward-column)
  (define-key sql-results-mode-map "." 'scroll-left)
  (define-key sql-results-mode-map " " 'sql-scroll-up)
  (define-key sql-results-mode-map "\C-v" 'sql-scroll-up)
  (define-key sql-results-mode-map "b" 'sql-scroll-down)
  (define-key sql-results-mode-map "\M-v" 'sql-scroll-down)
  (define-key sql-results-mode-map [(delete)] 'sql-scroll-down)
  (define-key sql-results-mode-map [(backspace)] 'sql-scroll-down)
  (define-key sql-results-mode-map "\n" 'sql-scroll-up-one-line)
  (define-key sql-results-mode-map "\r" 'sql-scroll-up-one-line)
  (define-key sql-results-mode-map "=" 'what-line)
  (define-key sql-results-mode-map "x" 'exchange-point-and-mark)
  (define-key sql-results-mode-map "s" 'sql-split-window-horizontally)
  (define-key sql-results-mode-map "d" 'sql-other-window-done)
  (define-key sql-results-mode-map "u" 'sql-edit-row)
  (define-key sql-results-mode-map "i" 'sql-iconify-frame)
  (define-key sql-results-mode-map "g" 'sql-grow-window-horizontally)
  (define-key sql-results-mode-map "n" 'shrink-window-horizontally)
;  (define-key sql-results-mode-map "r" 'isearch-backward)
  (define-key sql-results-mode-map "p" 'sql-pop-and-rename-buffer)
  (define-key sql-results-mode-map "h" 'sql-move-header-toggle)
;  (define-key sql-results-mode-map "f" 'sql-forward-column)
;  (define-key sql-results-mode-map "b" 'sql-backward-column)
  (define-key sql-results-mode-map "k" 'sql-cut-row)
  (define-key sql-results-mode-map "w" 'sql-cut-region)
  (define-key sql-results-mode-map "W" 'sql-copy-data)
  (define-key sql-results-mode-map "\C-y" 'sql-paste)
  (define-key sql-results-mode-map "\C-ce" 'sql-edit-toggle)
  (define-key sql-results-mode-map "e" 'sql-edit-toggle)
;  (define-key sql-results-mode-map "\C-B" 'sql-backward-column)
  (define-key sql-results-mode-map [(control F)] 'sql-forward-column)
  (define-key sql-results-mode-map [(control B)] 'sql-backward-column)
  (define-key sql-results-mode-map "\C-b" 'scroll-right)
  (define-key sql-results-mode-map "\C-f" 'scroll-left)
  (define-key sql-results-mode-map "\C-a" 'sql-beginning-of-row)
  (define-key sql-results-mode-map "\C-e" 'sql-end-of-row)
  (define-key sql-results-mode-map "l" 'sql-recenter)
  (define-key sql-results-mode-map [(control L)] 'sql-reposition-windows)
  (define-key sql-results-mode-map "\C-l" 'sql-recenter)
  (define-key sql-results-mode-map "o" 'sql-goto-batch-buffer)
  (define-key sql-results-mode-map [(right)] 'scroll-left)
  (define-key sql-results-mode-map [(left)] 'scroll-right)
  (define-key sql-results-mode-map [(shift right)] 'sql-forward-column)
  (define-key sql-results-mode-map [(shift left)] 'sql-backward-column)
  (define-key sql-results-mode-map [(up)] 'sql-scroll-down)
  (define-key sql-results-mode-map [(down)] 'sql-scroll-up)
  (define-key sql-results-mode-map [(shift up)] 'sql-scroll-down-one-line)
  (define-key sql-results-mode-map [(shift down)] 'sql-scroll-up-one-line)
  (define-key sql-results-mode-map [(shift button1)] 'sql-drag-display)
  (define-key sql-results-mode-map [(shift button2)] 'sql-yank-under-point)
  (define-key sql-results-mode-map [(button2)] 'sql-magic-yank-under-point)
  (define-key sql-results-mode-map "\C-c\C-q" 'sql-exit-sql-mode)
  (define-key sql-results-mode-map "\C-ch" 'sql-help-command)
  (define-key sql-results-mode-map "\M-p" 'sql-previous-history)
  (define-key sql-results-mode-map "\M-n" 'sql-next-history)
  (define-key sql-results-mode-map "\M-m" 'sql-previous-matching-history)
  (define-key sql-results-mode-map "\M-P" 'sql-previous-global-history)
  (define-key sql-results-mode-map "\M-N" 'sql-next-global-history))

(defvar sql-results-edit-map ()
  "Keymap used while in an sql-results-mode buffer in edit mode.")
(if sql-results-edit-map
    ()
  (setq sql-results-edit-map (make-keymap))
  (and sql-lucid (set-keymap-name sql-results-edit-map 'sql-results-edit-map))
  (define-key sql-results-edit-map "\C-c\C-p" 'sql-pop-and-rename-buffer)
  (define-key sql-results-edit-map "\C-ce" 'sql-edit-toggle)
  (define-key sql-results-edit-map "\M-b" 'scroll-right)
  (define-key sql-results-edit-map "\M-f" 'scroll-left)
  (define-key sql-results-edit-map "\C-a" 'sql-beginning-of-row)
  (define-key sql-results-edit-map "\C-e" 'sql-end-of-row)
  (define-key sql-results-edit-map "\C-d" 'sql-insert-a-space)
  (define-key sql-results-edit-map [(delete)] 'sql-backward-insert-a-space)
  (define-key sql-results-edit-map [(backspace)] 'sql-backward-insert-a-space)
;  (define-key sql-results-edit-map [(control l)] 'sql-recenter)
  (define-key sql-results-edit-map [(control L)] 'sql-reposition-windows)
  (define-key sql-results-edit-map "\C-l" 'sql-recenter)
  (define-key sql-results-edit-map [(control right)] 'scroll-left)
  (define-key sql-results-edit-map [(control left)] 'scroll-right)
  (define-key sql-results-edit-map [(shift right)] 'sql-forward-column)
  (define-key sql-results-edit-map [(shift left)] 'sql-backward-column)
  (define-key sql-results-edit-map [(shift button1)] 'sql-drag-display)
  (define-key sql-results-edit-map "\C-c\C-k" 'sql-delete-row)
  (define-key sql-results-edit-map "\C-c\C-q" 'sql-exit-sql-mode)
  (define-key sql-results-edit-map "\M-i" 'sql-update)
  (define-key sql-results-edit-map "\C-c\C-e" 'sql-update)
  (define-key sql-results-edit-map "\C-ch" 'sql-help-command))

(defun sql-mode-version ()
  "Return string describing the version of SQL Mode.  When called interactively,
displays the version in the echo area."
  (interactive)
  (if (interactive-p)
      (message "SQL Mode version %s" (sql-mode-version))
    sql-mode-version))

;;;###autoload
(defun sql-mode ()
  "Major mode for editing SQL code.
sql-mode Revision: 0.922 (beta)

To submit a bug report, enter `\\[sql-submit-bug-report]' from an
sql-mode, an sql-batch-mode, an sql-interactive-mode or an
sql-results-mode buffer.  This automatically sets up a mail buffer
with version information already added.  You just need to add a
description of the problem, including a reproducable test case
and send the message.  To send the message, enter `\\[mail-send-and-exit]'.

On entry to this mode, the hook variable `sql-mode-hook' is run with
no args, if that variable is bound and has a non-nil value.

Comments are delimited with /* ... */.

Key bindings:
\\{sql-mode-map}"
  (interactive)
  (setq major-mode 'sql-mode)
  (setq mode-name "SQL")
  (use-local-map sql-mode-map)
  (set-syntax-table sql-mode-syntax-table)
  (if sql-lucid
;      (if sql-use-big-menus
;	  (sql-turn-on-big-menus)
      (progn
	(set-buffer-menubar (copy-sequence current-menubar))
	(add-menu nil "SQL" (cdr sql-mode-menu) "Buffers"))
    (easy-menu-define sql-menu (list sql-mode-map) "SQL" sql-mode-menu))
;  (make-local-variable 'indent-line-function)
;  (setq indent-line-function 'sql-indent-line)
  (make-local-variable 'comment-start)
  (setq comment-start "/* ")
  (make-local-variable 'comment-end)
  (setq comment-end " */")
  (sql-font-lock)
  (and sql-abbrev-mode
       (progn
	 (sql-load-abbrevs)
	 (abbrev-mode 1)))
;  (and sql-use-toolbar (sql-turn-on-toolbar))
  (message nil)
  (run-hooks 'sql-mode-hook))

(defun sql-batch-mode (&optional server-name user-name user-password
				 the-database no-create)
  "Major mode for editing batch SQL commands.
sql-batch-mode Revision: 0.922 (beta)

To submit a bug report, enter `\\[sql-submit-bug-report]' from an
sql-mode, an sql-batch-mode, an sql-interactive-mode or an
sql-results-mode buffer.  This automatically sets up a mail buffer
with version information already added.  You just need to add a
description of the problem, including a reproducable test case
and send the message.  To send the message, enter `\\[mail-send-and-exit]'.

On entry to this mode, the hook variable `sql-batch-mode-hook' is run with
no args, if that variable is bound and has a non-nil value.

To evaluate the current buffer, use `\\[sql-evaluate-buffer]'

Comments are delimited with /* ... */.

Key bindings:
\\{sql-batch-mode-map}"
  (interactive)
  (setq server-name (or server-name
			(completing-read "Server Name: " sql-server-table)))
  (setq user-name (or user-name
		      (completing-read (format "User Name on Server %s: " 
					       server-name)
				       sql-user-table)))
  (setq new-password (or user-password
			 (let ((prompt (format "Password for %s on %s: "
					       user-name server-name)))
			   (if sql-secure-passwords
			       (sql-read-password prompt)
			     (read-string prompt)))))
  (setq sql-original-frame (selected-frame))
  (if no-create
      (progn
	(setq sql-old-contents (buffer-substring (point-min) (point-max)))
	(setq sql-old-history sql-history)
	(setq sql-matching-buffer nil)
	(kill-buffer nil)))
  (let* ((name (concat "+" server-name "+" user-name "+"
		       (if the-database
			   (concat the-database "+")
			 "")))
	 (new-buffer (not (buffer-live-p (get-buffer name))))
	 (sql-buf (get-buffer-create name)))
    (set-buffer sql-buf)
    (if (and sql-batch-in-new-frame
	     (null sql-batch-frame))
	(progn
	  (make-variable-buffer-local 'sql-batch-frame)
	  (setq sql-batch-frame (sql-make-batch-frame sql-buf))))
    (setq major-mode 'sql-batch-mode)
    (setq mode-name "SQL Batch")
    (switch-to-buffer sql-buf)
    (if no-create
	(progn
	  (insert sql-old-contents)
	  (setq sql-history sql-old-history)))
    (make-variable-buffer-local 'sql-server)
    (make-variable-buffer-local 'sql-user)
    (make-variable-buffer-local 'sql-password)
    (make-variable-buffer-local 'sql-database)
    (make-variable-buffer-local 'sql-database-name)
    (make-variable-buffer-local 'sql-history)
    (make-variable-buffer-local 'sql-history-index)
    (make-variable-buffer-local 'sql-history-index-string)
    (make-variable-buffer-local 'sql-matching-buffer)
    (make-variable-buffer-local 'sql-startup-message-displayed)
    (make-variable-buffer-local 'sql-table-list)
    (make-variable-buffer-local 'sql-database-list)
    (make-variable-buffer-local 'sql-column-list)
    (make-variable-buffer-local 'sql-value-list)
    (make-variable-buffer-local 'sql-stored-procedure-list)
    (make-variable-buffer-local 'sql-user-list)
    (make-variable-buffer-local 'sql-modified-cache)
    (make-variable-buffer-local 'sql-modified-history)
    (make-variable-buffer-local 'frame-icon-title-format)
    (make-variable-buffer-local 'sql-process)
    (make-variable-buffer-local 'sql-logged-in)
    (use-local-map sql-batch-mode-map)
    (setq sql-server server-name)
    (setq sql-user user-name)
    (setq sql-password new-password)
    (and the-database
	 (sql-set-database the-database))
    (setq frame-icon-title-format sql-server)
    (set-syntax-table sql-mode-syntax-table)
    (if sql-lucid
	(if sql-use-big-menus
	    (sql-turn-on-big-menus)
	(set-buffer-menubar (copy-sequence current-menubar))
	(add-menu nil "SQL" (cdr sql-batch-mode-menu) nil))
      (easy-menu-define sql-batch-menu (list sql-batch-mode-map) "SQL"
			sql-batch-mode-menu))
    (sql-font-lock)
    (if sql-abbrev-mode
	(progn
	  (sql-load-abbrevs)
	  (abbrev-mode 1)))
    (setq modeline-format sql-modeline-format)
    (message nil)
    (setq sql-frame (selected-frame))
    (setq batch-buffer (current-buffer))
    (if (not (buffer-live-p sql-matching-buffer))
	(sql-create-matching-buffer (current-buffer)))
    (set-buffer batch-buffer)
    (sql-position-results-buffer sql-matching-buffer)
    (sql-goto-batch-buffer)
    (and sql-use-toolbar (sql-turn-on-toolbar))
    (and sql-use-top-ten-toolbar (sql-turn-on-top-ten-toolbar))
    (and new-buffer sql-always-load-cache (sql-load-cache-data))
    (and new-buffer sql-always-load-history (sql-load-history))
    (and sql-save-cache-on-exit
	 (progn
	   (make-variable-buffer-local 'kill-buffer-hook)
	   (add-hook 'kill-buffer-hook 'sql-save-cache-data)
	   (add-hook 'kill-emacs-hook 'sql-save-cache-data-all-buffers)))
    (and sql-save-history-on-exit
	 (progn
	   (make-variable-buffer-local 'kill-buffer-hook)
	   (add-hook 'kill-buffer-hook 'sql-save-history)
	   (add-hook 'kill-emacs-hook 'sql-save-history-all-buffers)))
    (and sql-batch-in-new-frame
	 (progn
	   (make-variable-buffer-local 'kill-buffer-hook)
	   (add-hook 'kill-buffer-hook 'sql-maybe-delete-frame)))
    (sql-maybe-log-in)
    (run-hooks 'sql-batch-mode-hook)
    (if (not sql-inhibit-startup-message)
	(progn
	  (sql-display-startup-message)
	  (if (not (input-pending-p))
	      (message nil))))))

(defun sql-interactive-mode (&optional server-name user-name user-password
				       no-create)
  "Major mode for interacting with SQL servers.
sql-interactive-mode Revision: 0.922 (beta)

To submit a bug report, enter `\\[sql-submit-bug-report]' from an
sql-mode, an sql-batch-mode, an sql-interactive-mode or an
sql-results-mode buffer.  This automatically sets up a mail buffer
with version information already added.  You just need to add a
description of the problem, including a reproducable test case
and send the message.  To send the message, enter `\\[mail-send-and-exit]'.

On entry to this mode, the hook variable `sql-interactive-mode-hook' is
run with no args, if that variable is bound and has a non-nil value.

To evaluate the current command, type `go' followed by RETURN.

Comments are delimited with /* ... */.

Key bindings:
\\{sql-interactive-mode-map}"
  (interactive)
  (setq server-name (or server-name
			(completing-read "Server Name: " sql-server-table)))
  (setq user-name (or user-name
			(completing-read (format "User Name on Server %s: " 
						 server-name)
					 sql-user-table)))
  (setq new-password (or user-password 
			 (let ((prompt (format "Password for %s on %s: "
					       user-name server-name)))
			   (if sql-secure-passwords
			       (sql-read-password prompt)
			     (read-string prompt)))))
  
;Usage: SQLPLUS [<option>] [<user>[/<password>] [@<host>]]
;	        [@<startfile> [<parm1>] [<parm2>] ...]
;where <option> ::= { -s | -? }
;-s for silent mode and -? to obtain version number

  (let ((new-buffer nil)
	(new-buffer-name (concat server-name "-" user-name)))
    (cond
     ((eq sql-dataserver-type 'sybase)
      (if sql-interactive-command-switches
	  (setq new-buffer
		(make-comint new-buffer-name
			     sql-command nil
			     (concat "-w" (int-to-string sql-max-frame-width))
			     (concat "-U" user-name) 
			     (concat "-P" new-password)
			     (concat "-S" server-name)
			     sql-interactive-command-switches))
	(setq new-buffer
	      (make-comint new-buffer-name
			   sql-command nil
			   (concat "-w" (int-to-string sql-max-frame-width))
			   (concat "-U" user-name) 
			   (concat "-P" new-password)
			   (concat "-S" server-name)))))
     ((eq sql-dataserver-type 'oracle)
      (if sql-interactive-command-switches
	  (setq new-buffer
		(make-comint new-buffer-name
			     sql-command nil
			     (concat user-name "/" new-password)
			     (concat "@" server-name)
			     sql-interactive-command-switches))
	(setq new-buffer
	      (make-comint new-buffer-name
			   sql-command nil
			   (concat user-name "/" new-password)
			   (concat "@" server-name)))))
     (t
      (error "Unrecognized database type `%s'." sql-dataserver-type)))
    (set-buffer new-buffer)
    (if (and sql-batch-in-new-frame
	     (null sql-batch-frame))
	(progn
	  (make-variable-buffer-local 'sql-batch-frame)
	  (setq sql-batch-frame (sql-make-batch-frame sql-buf))))
    (switch-to-buffer new-buffer))
  (kill-all-local-variables)
  (make-variable-buffer-local 'sql-server)
  (make-variable-buffer-local 'sql-user)
  (make-variable-buffer-local 'sql-password)
  (make-variable-buffer-local 'sql-database)
  (make-variable-buffer-local 'sql-database-name)
  (make-variable-buffer-local 'sql-history)
  (make-variable-buffer-local 'sql-history-index)
  (make-variable-buffer-local 'sql-history-index-string)
  (make-variable-buffer-local 'sql-command-level)
  (make-variable-buffer-local 'next-line-add-newlines)
  (make-variable-buffer-local 'frame-icon-title-format)
  (comint-mode)
  (use-local-map sql-interactive-mode-map)
  (setq comint-prompt-regexp "^.*> ")  
  (set-syntax-table sql-mode-syntax-table)
  (if sql-lucid
;      (if sql-use-big-menus
;	  (sql-turn-on-big-menus)
      (progn
	(set-buffer-menubar (copy-sequence current-menubar))
	(add-menu nil "SQL" (cdr sql-interactive-mode-menu) nil))
    (easy-menu-define sql-interactive-menu (list sql-interactive-mode-map) "SQL"
		      sql-interactive-mode-menu))
  (setq major-mode 'sql-interactive-mode)
  (setq mode-name "SQL Interaction")
  (setq next-line-add-newlines nil)
  (make-variable-buffer-local 'isearch-mode-hook)
  (make-variable-buffer-local 'isearch-mode-end-hook)
  (add-hook 'isearch-mode-hook 'sql-isearch-begin)
  (add-hook 'isearch-mode-end-hook 'sql-isearch-end)
  (sql-font-lock)
  (if sql-abbrev-mode
      (progn
	(sql-load-abbrevs)
	(abbrev-mode 1)))
  (setq truncate-lines t)
  (setq sql-server server-name)
  (setq sql-user user-name)
  (setq sql-password new-password)
  (setq modeline-format sql-modeline-format)
  (setq frame-icon-title-format sql-server)
;  (and sql-use-toolbar (sql-turn-on-toolbar))
  (message nil)
  (and sql-always-load-cache (sql-load-cache-data))
  (and sql-always-load-history (sql-load-history))
  (and sql-save-cache-on-exit
       (progn
	 (make-variable-buffer-local 'kill-buffer-hook)
	 (add-hook 'kill-buffer-hook 'sql-save-cache-data)
	 (add-hook 'kill-emacs-hook 'sql-save-cache-data-all-buffers)))
  (and sql-save-history-on-exit
       (progn
	 (make-variable-buffer-local 'kill-buffer-hook)
	 (add-hook 'kill-buffer-hook 'sql-save-history)
	 (add-hook 'kill-emacs-hook 'sql-save-history-all-buffers)))
  (run-hooks 'sql-interactive-mode-hook)
  (if (not sql-inhibit-startup-message)
      (progn
	(sql-display-startup-message)
	(if (not (input-pending-p))
	    (message nil)))))

(defun sql-results-mode (&optional the-server the-user bare)
  "Major mode for viewing SQL results.
sql-results-mode Revision: 0.922 (beta)

sql-results-mode is similar to view mode.  The following commands are
available to browse the results buffer.  If you want to do editing in the
results buffer, you should enter the sql-results-edit mode (see below).

If your query returns more than one batch of results, you will probably
want to turn off the header shifting.  You can accomplish this with the
`h' key.

A command summary of the keybindings that are available are as follows:

	Key Binding		Action

	SPACE			Scroll down one screenfull
	DOWN ARROW		Scroll down one screenfull
	b 			Scroll up one screenfull
	BACKSPACE		Scroll up one screenfull
	DELETE			Scroll up one screenfull
	UP ARROW		Scroll up one screenfull
	,			Scroll left one screenfull
	<			Scroll left one screenfull
	LEFT ARROW		Scroll left one screenfull
	.			Scroll right one screenfull
	>			Scroll right one screenfull
	RIGHT ARROW		Scroll right one screenfull
	RETURN			Scroll down one line
	LINEFEED		Scroll down one line
	SHIFT DOWN ARROW	Scroll down one line
	SHIFT UP ARROW		Scroll up one line
	SHIFT LEFT ARROW	Scroll left one column
	SHIFT RIGHT		Scroll right one column
	g			Goto line
	=			Display current line
	x			Exchange point and mark
	s			Forward incramental search
	r			Reverse incramental search
	p			Pop and rename buffer
	h			Toggle the shifting of headers
	e			Edit mode
	C-c e			Edit mode
	C-a			Beginning of row
	C-e			End of row
	l			Recenter
	o			Other window

	SHIFT button1		Drag display

	For backwards compatibility:

	C-v			Scroll down one screenfull
	M-v			Scroll up one screenfull
	C-b			Scroll left one screenfull
	C-f			Scroll right one screenfull
	C-L			Recenter
	
To submit a bug report, enter `\\[sql-submit-bug-report]' from an
sql-mode, an sql-batch-mode, an sql-interactive-mode or an
sql-results-mode buffer.  This automatically sets up a mail buffer
with version information already added.  You just need to add a
description of the problem, including a reproducable test case
and send the message.  To send the message, enter `\\[mail-send-and-exit]'.

On entry to this mode, the hook variable `sql-results-mode-hook' is run with
no args, if that variable is bound and has a non-nil value.

Key bindings:
\\{sql-results-mode-map}

Whie in EDIT mode:
\\{sql-results-edit-map}"
  (interactive)
  (setq major-mode 'sql-results-mode)
  (setq mode-name "SQL Results")
  (setq truncate-lines t)
  (make-variable-buffer-local 'isearch-mode-hook)
  (make-variable-buffer-local 'isearch-mode-end-hook)
  (add-hook 'isearch-mode-hook 'sql-isearch-begin)
  (add-hook 'isearch-mode-end-hook 'sql-isearch-end)
  (make-variable-buffer-local 'sql-history)
  (make-variable-buffer-local 'sql-history-index)
  (make-variable-buffer-local 'sql-history-index-string)
  (make-variable-buffer-local 'sql-matching-buffer)
  (make-variable-buffer-local 'sql-one-query)
  (make-variable-buffer-local 'sql-header-text)
  (make-variable-buffer-local 'sql-current-error-point)
  (if (and the-server the-user)
      (progn
	(make-variable-buffer-local 'sql-server)
	(make-variable-buffer-local 'sql-user)
	(setq sql-server the-server)
	(setq sql-user the-user)))
  (make-variable-buffer-local 'frame-icon-title-format)
  (make-variable-buffer-local 'sql-results-view-mode)
  (make-variable-buffer-local 'sql-results-edit-mode)
  (make-variable-buffer-local 'sql-results-mode-editing)
  (make-variable-buffer-local 'sql-linked-windows)
  (setq frame-icon-title-format sql-server)
  (sql-results-view-mode)
  (use-local-map sql-results-mode-map)
  (set-syntax-table sql-mode-syntax-table)
  (sql-font-lock)
  (if (and sql-lucid (not bare))
      (if sql-use-big-menus
	  (sql-turn-on-big-menus)
	(set-buffer-menubar (copy-sequence current-menubar))
	(add-menu nil "SQL" (cdr sql-results-mode-menu) nil))
    (easy-menu-define sql-results-menu (list sql-results-mode-map) "SQL"
		      sql-results-mode-menu))
  (and (not bare)
       (> emacs-minor-version 11)
       sql-use-toolbar
       (sql-turn-on-toolbar))
  (and (not bare)
       (> emacs-minor-version 11)
       sql-use-top-ten-toolbar
       (sql-turn-on-top-ten-toolbar))
  (setq modeline-format sql-modeline-format)
  (message nil)
  (run-hooks 'sql-results-mode-hook))

(defun sql-results-view-mode ()
  "Minor mode to view results."
  (interactive)
  (use-local-map sql-results-mode-map)
  (setq sql-results-mode-editing nil)
  (setq sql-results-view-mode t)
  (setq sql-results-edit-mode nil)
  (overwrite-mode 0)
  (remove-hook 'post-command-hook 'sql-edit-row-sentinel)
  (and sql-mark-changes
      (sql-stop-marking-changes))
  (redraw-modeline))

(defun sql-results-edit-mode ()
  "Minor mode to edit results."
  (interactive)
  (use-local-map sql-results-edit-map)
  (setq sql-results-mode-editing t)
  (setq sql-results-view-mode nil)
  (setq sql-results-edit-mode t)
  (overwrite-mode 1)
  (and sql-mark-changes
      (sql-start-marking-changes))
  (redraw-modeline))

(defun sql-edit-toggle ()
  "Toggle the state of editing in the current results buffer."
  (interactive)
  (if sql-results-mode-editing
      (sql-results-view-mode)
    (sql-results-edit-mode)))

(defun sql-create-matching-buffer (this-buffer)
  (setq buffer
	(if (eq major-mode 'sql-batch-mode)
	    (get-buffer-create (concat "+" sql-server "-" sql-user " results+"))
	  (get-buffer-create (concat "=" sql-server "=" sql-user "=results="))))
  (setq sql-matching-buffer buffer)
  (let ((the-server sql-server)
	(the-user sql-user))
    (set-buffer buffer)
    (setq sql-matching-buffer this-buffer)
    (sql-results-mode the-server the-user (eq major-mode 'sql-mode))
    (sql-stop-marking-changes)
    (erase-buffer)))

(defun sql-other-window-done (arg)
  "Switch to the other window and clear the results and batch buffers.
With prefix ARG, don't clear the results buffer."
  (interactive "P")
  (other-window 1)
  (sql-goto-history 0 arg))

(defun sql-filter (proc string)
  "Process filter for results buffers.
Just inserts the text, but uses `insert-before-markers'."
  (save-excursion
    (if sql-getting-completions
	(set-buffer sql-completion-buffer)
      (set-buffer (process-buffer proc)))
    (goto-char (point-max))
    (insert string)
    (if (save-excursion (beginning-of-line)
			(looking-at sql-first-prompt-regexp))
	(if sql-getting-completions
	    (setq sql-process-busy nil)
	  (let ((prompt-count 0))
	    (goto-char (point-min))
	    (while (search-forward sql-first-prompt-regexp nil t)
	      (setq prompt-count (1+ prompt-count)))
	    (if (>= prompt-count sql-go-count)
		(progn
		  (goto-char (point-min))
		  (while (re-search-forward sql-all-prompts-regexp nil t)
		    (replace-match "" nil nil))
		  (goto-char (point-min))
		  (setq sql-process-busy nil)
		  (if (not sql-logged-in)
		      (progn
			(sql-set-logged-in t)
			(erase-buffer)
			(insert "\n\n\n\nSuccessful login to dataserver "
				sql-server " as " sql-user".")
			(let ((fill-column (window-width)))
			  (center-line)))
		    (let ((silent nil))
		      (sql-finish-evaluating-buffer (current-buffer)))))))))))

(defun sql-sentinel (proc msg)
  "Sentinel for results buffers."
  (setq modeline-process
	(if (not sql-stay-logged-in)
	    nil
	  (concat ": " (symbol-name (process-status proc)))))
  (let ((buffer (process-buffer proc))
	(old-frame (selected-frame)))
    (if (memq (process-status proc) '(signal exit))
	(progn
	  (if (null (buffer-name buffer))
	      ;; buffer killed
	      (set-process-buffer proc nil)
	    (let ((obuf (current-buffer))
		  omax opoint)
	      ;; save-excursion isn't the right thing if
	      ;; process-buffer is current-buffer
	      (unwind-protect
		  (progn
		    ;; Write something in the results buffer
		    ;; and hack its mode line.
		    (set-buffer buffer)
		    (goto-char (point-min))
		    (sql-goto-batch-buffer)
 ;		    (and sql-reposition-windows-when-done
 ;			 (sql-reposition-windows))
		    (if (or (eq sql-evaluation-method 'background)
			    sql-asynchronous-flag)
			(progn
			  (let ((silent nil))
			    (setq sql-asynchronous-flag nil)
			    (sql-finish-evaluating-buffer sql-matching-buffer)
			    (and (member 'ding sql-finished-async-query-options)
				 (play-sound 'sql-ready))
			    (and (member 'open sql-finished-async-query-options)
			       (deiconify-frame sql-frame))
			    (and (member 'raise sql-finished-async-query-options)
				 (raise-frame sql-frame))))))
		(set-buffer obuf))))
	  (setq sql-query-in-progress (delq proc sql-query-in-progress))
	  ))
    (select-frame old-frame)))

;(defun sql-sentinel (proc msg)
;  (set-buffer (process-buffer proc))
;  (sql-goto-batch-buffer)
;  (sql-finish-evaluating-buffer nil)
;  (message msg))

(defun sql-evaluate-region ()
  "Send the contents of the current region to an SQL process.
This function simply invokes sql-evaluate-buffer with an argument to
specify a region instead of the whole buffer."
  (sql-evaluate-buffer t))

;(defun sql-evaluate-buffer-other-way ()
;  (interactive)
;  (cond
;   ((eq sql-evaluation-method 'background)
;    (let ((sql-evaluation-method 'foreground))
;      (sql-evaluate-buffer nil)))
;   ((eq sql-evaluation-method 'foreground)
;    (let ((sql-evaluation-method 'background))
;      (sql-evaluate-buffer nil)))))

(defun sql-evaluate-buffer-asynchronous ()
  "Send the contents of the buffer to an SQL process asynchronously.
With prefix ARG, send the contents of the region in the current buffer
to an SQL process.

On entry to this function, the hook variable `sql-evaluate-buffer-hook'
is run with no args, if that variable is bound and has a non-nil value."
  (interactive)
  (setq sql-asynchronous-flag t)
  (let ((sql-evaluation-method 'background))
    (sql-evaluate-buffer nil)))

(defun sql-evaluate-buffer (flag &optional silent)
  "Send the contents of the buffer to an SQL process.
With prefix ARG, send the contents of the region in the current buffer
to an SQL process.

On entry to this function, the hook variable `sql-evaluate-buffer-hook'
is run with no args, if that variable is bound and has a non-nil value."
  (interactive "P")
  (or (eq major-mode 'sql-mode) (set-buffer-modified-p nil))
  (setq sql-old-point (point))
  (setq sql-frame (selected-frame))
  (if (not silent)
      (progn
	(sql-check-for-where)
	(if sql-confirm-changes
	    (if (sql-get-rows-affected flag)
		nil
	      (error "Aborted.")))))
  (if sql-database
      (message "Getting results from %s on %s..." sql-database sql-server)
    (message "Getting results from %s..." sql-server))
  (setq sql-last-command (buffer-substring (point-min) (point-max)))
;  (or silent (sql-add-buffer-to-history t))
  (if (not (buffer-live-p sql-matching-buffer))
      (sql-create-matching-buffer (current-buffer)))
  (set-buffer sql-matching-buffer)
  (sql-results-view-mode)
  (sql-stop-marking-changes)
  (erase-buffer)
  (set-buffer sql-matching-buffer)
  (run-hooks 'sql-before-evaluate-buffer-hook)
  (let* ((results-buffer sql-matching-buffer)
	 (start (if flag (point) (point-min)))
	 (end (if flag (mark) (point-max)))
	 (switches (if (or sql-batch-command-switches sql-intersperse-headers
			   sql-column-separator)
		       (concat
			sql-batch-command-switches
			(if sql-intersperse-headers
			    (concat "-h "
				    (- (frame-height)
				       (sql-calculate-results-buffer-height)
				       next-screen-context-lines
				       5))
			  "")
			(if sql-column-separator
			    (concat "-s" sql-column-separator)
			  ""))))
	 (command (buffer-substring start end)))
;    (message switches) (sit-for 4)
    (and sql-database
	 (setq command (concat "use " sql-database "\ngo\n" command)))
    (and sql-require-final-go
	 (setq command (concat command "\ngo\n")))
    (sql-evaluate command sql-server sql-user sql-password switches
		  results-buffer silent))
  (and (member 'ding sql-finished-query-options)
       (play-sound 'sql-ready))
  (and (member 'open sql-finished-query-options)
       (deiconify-frame sql-frame))
  (and (member 'raise sql-finished-query-options)
       (raise-frame)))

(defun sql-evaluate (string server user password switches output-buffer
			    silent)
  "Evaluate STRING on SQL Server SERVER using login USER and PASSWORD.
If SWITCHES is non-nil, it is passed as a command-line argument to sql-command.
Output from the evaluation is put in OUTPUT-BUFFER.

Method of evaluation is determined by the variable `sql-evaluation-method'."
  (cond
   ((eq sql-evaluation-method 'foreground)
    (sql-evaluate-foreground string server user password switches
			     output-buffer)
    (or silent (sql-finish-evaluating-buffer output-buffer)))
   ((eq sql-evaluation-method 'background)
    (sql-evaluate-background string server user password switches
			     output-buffer))
   (t
    (error "Unknown evaluation method."))))

(defun sql-evaluate-foreground (string server user password switches
				       output-buffer)
  "Evaluate STRING on SQL Server SERVER using login USER and PASSWORD.
If SWITCHES is non-nil, it is passed as a command-line argument to sql-command.
Output from the evaluation is put in OUTPUT-BUFFER."
  (if sql-stay-logged-in
      (sql-process-send-string (get-buffer-process output-buffer) string)
    (let ((temp-eval-buffer (get-buffer-create " SQL-TEMP-EVAL")))
      (if sql-getting-completions
	  (setq output-buffer sql-completion-buffer))
      (set-buffer temp-eval-buffer)
      (erase-buffer)
      (insert string)
      (if switches
	  (call-process-region (point-min) (point-max) sql-command nil
			       output-buffer t 
			       (concat "-w" (int-to-string sql-max-frame-width))
			       (concat "-U" user) (concat "-P" password) 
			       (concat "-S" server)
			       switches)
	(call-process-region (point-min) (point-max) sql-command nil
			     output-buffer t 
			     (concat "-w" (int-to-string sql-max-frame-width))
			     (concat "-U" user) (concat "-P" password) 
			     (concat "-S" server)))
      (kill-buffer temp-eval-buffer))))
  
(defun sql-evaluate-background (string server user password switches
				       output-buffer)
  "Send the contents of the buffer asynchronously to an SQL process.
With prefix ARG, send the contents of the region in the current buffer
to an SQL process.

On entry to this function, the hook variable `sql-evaluate-buffer-hook'
is run with no args, if that variable is bound and has a non-nil value."
  (interactive "P")
  (if sql-stay-logged-in
      (sql-process-send-string (get-buffer-process output-buffer) string)
    (let ((command (concat sql-command " "
			   " -w" (int-to-string sql-max-frame-width)
			   " -U" user
			   " -P" password
			   " -S" server
			   " < " "/tmp/SQL_MODE_TEMP_FILE")))
      (write-region string nil "/tmp/SQL_MODE_TEMP_FILE" nil 1)
;      (set-buffer batch-buffer)
      (setq sql-process (start-process-shell-command (downcase mode-name)
						     output-buffer
						     command))
      (set-process-sentinel sql-process 'sql-sentinel)
      (setq modeline-process
	    (concat ": " (symbol-name (process-status sql-process))))
;      (set-process-filter sql-process 'sql-filter)
      (set-marker (process-mark sql-process) (point) sql-matching-buffer)
      (setq sql-query-in-progress (cons sql-process sql-query-in-progress)))))

(defun sql-set-stay-logged-in (flag)
  (interactive)
  (setq sql-stay-logged-in flag)
  (if flag
      (sql-log-in)
    (sql-log-out)))

(defun sql-set-logged-in (flag)
  (interactive)
  (setq sql-logged-in flag)
  (let ((this-buffer (current-buffer)))
    (set-buffer sql-matching-buffer)
    (setq sql-logged-in flag)
    (set-buffer this-buffer)))
   
(defun sql-maybe-log-in ()
  (and (or sql-log-in-on-start
	   sql-stay-logged-in)
       (sql-log-in)))

(defun sql-log-in ()
  (if (and sql-process
	   (processp sql-process)
	   (eq (process-status sql-process) 'run))
      ()
    (progn
      (setq sql-process
	    (start-process (downcase mode-name)
			   sql-matching-buffer
			   sql-command
			   (concat "-w" (int-to-string
					 sql-max-frame-width))
			   (concat "-U" sql-user)
			   (concat "-P" sql-password)
			   (concat "-S" sql-server)))
      (set-buffer sql-matching-buffer)
      (sql-set-logged-in nil)
      (set-buffer sql-matching-buffer)
      (set-process-sentinel sql-process 'sql-sentinel)
      (setq modeline-process
	    (concat ": " (symbol-name (process-status sql-process))))
      (set-process-filter sql-process 'sql-filter)
      (set-marker (process-mark sql-process) (point) sql-matching-buffer)
      (if (not (eq (process-status sql-process) 'run))
	  (error (concat "Could not start " sql-command " process."))
	sql-process))))

(defun sql-log-out ()
  "Close the connection with the dataserver."
  (interactive)
  (sql-goto-batch-buffer)
  (and sql-process
       (processp sql-process)
       (progn
	 (kill-process sql-process)
	 (setq modeline-process nil)
	 (sql-set-logged-in nil)
	 (message "Process terminated."))))

(defun sql-process-send-string (process string)
  (if (or (not process)
	  (not (processp process))
	  (not (eq (process-status process) 'run)))
      (setq process (sql-log-in)))
  (save-excursion
    (goto-char (point-min))
    (setq sql-go-count 1)
    (while (re-search-forward "\\<go\n" nil t)
      (setq sql-go-count (1+ sql-go-count))))
  (process-send-string process string)
  (setq sql-process-busy t)
  (if (eq sql-evaluation-method 'foreground)
      (while sql-process-busy
	(accept-process-output))))

(defun sql-finish-evaluating-buffer (r-buffer)
  (set-buffer r-buffer)
  (set-buffer sql-matching-buffer)
  (goto-char sql-old-point)
  (sql-position-results-buffer r-buffer)
  (set-buffer r-buffer)
  (setq sql-linked-windows (list (selected-window)))
;  (setq sql-history sql-old-history)
;  (setq sql-history-index sql-old-history-index)
  (if (not silent)
      (progn
	(setq sql-current-error-point nil)
	(sql-add-buffer-to-history t)))
  (goto-char (point-min))
  (if (sql-multi-headers)
      (setq sql-move-headers nil)
    (setq sql-move-headers t)
    (setq sql-header-text (buffer-substring (point-min)
					    (progn
					      (forward-line 2) (point)))))
  (if (member 'message sql-finished-query-options)
      (message (sql-get-status))
    (if sql-database
	(message "Getting results from %s on %s... done" sql-database sql-server)
      (message "Getting results from %s... done" sql-server )))
  (save-excursion
    (goto-char (point-max))
    (insert "\n\n"))
;  (sql-font-lock)
  (set-buffer-modified-p nil)
  (if (eq sql-evaluation-method 'background)
      ()
    (if (and sql-stay-in-batch-buffer (not silent))
	(progn
	  (sql-goto-batch-buffer)
	  (goto-char sql-old-point))
      (sql-goto-batch-buffer)
      (sql-goto-results-buffer)
      (goto-char (point-min))))
  (run-hooks 'sql-evaluate-buffer-hook))

(defun sql-toggle-current-font-lock ()
  "Toggle the state of font-lock-mode in the current buffer."
  (interactive)
  (if font-lock-mode
      (sql-turn-off-font-lock)
    (sql-turn-on-font-lock)))

(defun sql-font-lock ()
  (if (or (eq sql-font-lock-buffers 'all)
	  (member major-mode sql-font-lock-buffers))
      (sql-turn-on-font-lock)
    (sql-turn-off-font-lock)))

(defun sql-turn-on-font-lock ()
  (if (eq major-mode 'sql-results-mode)
      (cond
	(sql-xemacs-19-12
	 (set-face-foreground
	  'default
	  (specifier-instance (face-foreground 'sql-results-face))
	  (current-buffer)))
	(sql-xemacs
	 (let ((re (make-extent (point-min) (point-max))))
	   (set-extent-face re 'sql-results-face))))
    (font-lock-mode 1)))

(defun sql-turn-off-font-lock ()
  (font-lock-mode 0)
  (and (eq major-mode 'sql-results-mode)
       (cond
	(sql-xemacs-19-12
	 (remove-specifier (face-foreground 'default) (current-buffer)))
	(sql-xemacs
	 (and (extent-at (point-min))
	      (delete-extent (extent-at (point-min))))))))

(defun sql-multi-headers ()
  "Return t if it looks like there is not exactly one line of headers."
  (save-excursion
    (goto-char (point-min))
    (forward-line 1)
    (if (looking-at " ---")
	(progn
	  (forward-line 1)
	  (search-forward "---" nil t))
      t)))
   
(defun sql-calculate-results-buffer-height ()
  "Calculate the number of lines to allocate for the results buffer."
  (let* ((percent (/ (* (frame-height) sql-results-buffer-percent) 100))
	 (lines (- (frame-height) percent)))
    (if sql-greedy-results-buffers
	(setq lines (max window-min-height
			 (min lines 
			      (+ 2 (count-lines (point-min) (point-max)))))))
    lines))

(defun sql-is-window-configured ()
  (save-window-excursion
    (eq sql-matching-buffer
	(progn
	  (other-window 1)
	  (current-buffer)))))
   
(defun sql-position-results-buffer (buffer)
  "Place and resize the results buffer BUFFER.
The values of sql-results-buffer-percent and sql-greedy-results-buffers
are consulted."
  (if sql-results-in-new-frame
      (sql-put-results-in-new-frame buffer)
    (let ((old-frame (selected-frame)))
      (select-frame sql-frame)
      (if (sql-is-window-configured)
	  ()
	(delete-other-windows)
					;  (goto-char (point-min))
	(split-window nil (sql-calculate-results-buffer-height))
					;  (other-window 1)
	(switch-to-buffer-other-window buffer)
	(select-frame old-frame)))))

(defun sql-mark-changed (begin end l)
  (interactive)
  (and sql-lucid
       (let* ((line-start (save-excursion (beginning-of-line) (point)))
	      (line-end (save-excursion (end-of-line) (point)))
	      (line-extent (make-extent line-start line-end))
	      (area-extent (make-extent begin end)))
	 (set-extent-face line-extent 'sql-changed-line-face)
	 (set-extent-face area-extent 'sql-changed-area-face))))

(defun sql-start-marking-changes ()
  "Start marking changed areas with highlight."
  (interactive)
  (setq sql-marking-changes t)
  (make-local-variable 'old-after-change-function)
  (make-local-variable 'after-change-function)
  (setq old-after-change-function after-change-function)
  ;; there must be a better way
  (if old-after-change-function
      (setq after-change-function 
	    (list 'lambda '(a b c)
		  '(sql-mark-changed a b c)
		  (if old-after-change-function 
		      (list old-after-change-function 'a 'b 'c))))
    (setq after-change-function 'sql-mark-changed)))

(defun sql-stop-marking-changes ()
  "Stop marking changed areas with highlight.
See help on the variable `sql-mark-changes'."
  (interactive)
  (setq sql-marking-changes nil)
  (if (boundp 'old-after-change-function)
      (setq after-change-function old-after-change-function)))

(defun sql-toggle-marking-changes ()
  "Start or stop marking changes."
  (interactive)
  (if sql-marking-changes
      (sql-stop-marking-changes)
    (sql-start-marking-changes)))

(defun sql-toggle-big-menus ()
  "Expand or contract menus depending on current state."
  (interactive)
  (if sql-use-big-menus
      (sql-turn-off-big-menus)
    (sql-turn-on-big-menus)))

(defun sql-turn-off-big-menus ()
  (or (eq major-mode 'sql-batch-mode)
      (eq major-mode 'sql-results-mode)
      (error "You must be in a sql-batch-mode or sql-results-mode buffer."))
  (setq sql-use-big-menus nil)
  (sql-turn-off-big-menus-1)
  (if sql-matching-buffer
      (progn
	(set-buffer sql-matching-buffer)
	(sql-turn-off-big-menus-1)
	(set-buffer sql-matching-buffer))))

(defun sql-turn-off-big-menus-1 ()
  (set-buffer-menubar sql-old-menu)
  (sql-add-menus)
  (add-menu nil "SQL" (cond
		       ((eq major-mode 'sql-mode)
			(cdr sql-mode-menu))
		       ((eq major-mode 'sql-batch-mode)
			(cdr sql-batch-mode-menu))
		       ((eq major-mode 'sql-results-mode)
			(cdr sql-results-mode-menu))
		       ((eq major-mode 'sql-interactive-mode)
			(cdr sql-interactive-mode-menu))
		       (t
			(error "You must be in a SQL Mode buffer.")))
	    nil)
  (add-menu '("SQL" "Options") (car (car sql-font-lock-menu))
	    (cdr (car sql-font-lock-menu))
	    "SYBASE Environment Variable...")
  (set-menubar-dirty-flag))

(defun sql-turn-on-big-menus ()
  (interactive)
  (or (eq major-mode 'sql-batch-mode)
      (eq major-mode 'sql-results-mode)
      (error "You must be in a sql-batch-mode or sql-results-mode buffer."))
  (setq sql-use-big-menus t)
  (set-buffer-menubar sql-big-menu)
  (delete-menu-item '("Options" "Coloring"))
  (set-menubar-dirty-flag)
  (if sql-matching-buffer
      (progn
	(set-buffer sql-matching-buffer)
	(set-buffer-menubar sql-big-menu)
	(delete-menu-item '("Options" "Coloring"))
	(set-menubar-dirty-flag)
	(set-buffer sql-matching-buffer))))

(defun sql-use-emacs-menu ()
  (interactive)
  (set-buffer-menubar sql-old-menu)
  (if sql-xemacs-19-12
      (add-menu-button nil ["SQL" sql-turn-on-big-menus t] nil)
    (add-menu-item nil "SQL" 'sql-turn-on-big-menus t nil))
  (set-menubar-dirty-flag)
  (if sql-matching-buffer
      (progn
	(set-buffer sql-matching-buffer)
	(set-buffer-menubar sql-old-menu)
	(if sql-xemacs-19-12
	    (add-menu-button nil ["SQL" sql-turn-on-big-menus t] nil)
	  (add-menu-item nil "SQL" 'sql-turn-on-big-menus t nil))
	(set-menubar-dirty-flag)
	(set-buffer sql-matching-buffer))))

(defun sql-print-buffer-tiled ()
  "Format and print the current buffer, splitting pages as necessary."
  (interactive)
  (save-excursion
    (goto-char (point-max))
    (insert "\n")
    (let ((done nil)
	  (buffer1 (get-buffer-create " SQL-PRINTER-OUTPUT-1"))
	  (text nil)
	  (contents (buffer-substring (point-min) (point-max)))
	  (end nil)
	  (chars (point-max)))
      (set-buffer buffer1)
      (insert contents)
      (while (not done)
	(goto-char (point-min))
	(if (not (re-search-forward "[a-zA-Z0-9]" nil t))
	    (setq done t)
	  (goto-char (point-max))
	  (let ((width sql-print-characters-per-line))
	    (while (> width 0)
	      (setq width (1- width))
	      (insert " ")))
	  (setq end (point))
	  (setq text (delete-extract-rectangle 1 end))
	  (set-buffer (get-buffer-create "SQL-PRINTER-OUTPUT"))
	  (while text
	    (insert (car text))
	    (insert "\n")
	    (setq text (cdr text)))
	  (let ((lpr-switches (or sql-print-switches lpr-switches))
		(enscript-switches (or sql-print-switches enscript-switches)))
	    (if (eq sql-print-command 'enscript-buffer)
		(enscript-buffer nil)
	      (funcall sql-print-command)))
	  (kill-buffer "SQL-PRINTER-OUTPUT")
	  (set-buffer buffer1)
	  (message "Spooling... (%3d%%)"
		   (/ (* 100 (- chars (point-max))) chars))))
      (kill-buffer buffer1)
      (message "Spooling... done"))))

;      (erase-buffer)
;      (let ((marking-changes sql-marking-changes))
;	(sql-stop-marking-changes)
;	(insert contents)
;	(if marking-changes
;	    (sql-start-marking-changes))))))

(defun sql-get-status ()
  "Get the status returned in a results buffer, if it is returned."
  (goto-char (point-max))
  (if (eq 0 (forward-line -1))
      (let ((msg (buffer-substring (point) (1- (point-max)))))
	(goto-char (point-min))
	msg)
    ""))

(defun sql-put-results-in-new-frame (buffer)
  "Put the results buffer in it's own X frame."
  (if (frames-of-buffer buffer)
      (select-frame (car (frames-of-buffer buffer)))
    (let ((new-frame (make-frame)))
      (select-frame new-frame)
      (switch-to-buffer buffer)
      (make-variable-buffer-local 'sql-results-frame)
      (setq sql-results-frame new-frame)
      (if sql-resize-results-frames
	  (progn
	    (set-frame-height new-frame sql-results-frame-height)
	    (set-frame-width new-frame sql-results-frame-width)))
      new-frame)))

(defun sql-make-batch-frame (buffer)
  "Make a frame for sql-batch-mode."
  (let ((new-frame (make-frame)))
    (select-frame new-frame)
    (switch-to-buffer buffer)
    (if sql-resize-batch-frames
	(progn
	  (set-frame-height new-frame sql-batch-frame-height)
	  (set-frame-width new-frame sql-batch-frame-width)))
    new-frame))

(defun sql-add-buffer-to-history (global)
  "Add the contents of the current buffer to the history list.
If GLOBAL is non-nil, add it to the global history as well."
  (let ((last-results (buffer-substring (point-min) (point-max)))
	(this-buffer (current-buffer)))
    (set-buffer sql-matching-buffer)
    (if (equal (length sql-history) sql-history-length)
	(setq sql-history (reverse (cdr (reverse sql-history)))))
    (if (equal (length sql-global-history) sql-global-history-length)
	(setq sql-global-history (reverse (cdr (reverse sql-global-history)))))
    (setq sql-modified-history t)
    (setq sql-history (cons (cons sql-last-command last-results) sql-history))
    (setq sql-end-history "")
    (sql-set-history-index 1)
    (set-buffer this-buffer)
    (if (and global
	     (not (string-equal sql-last-command
				(car sql-global-history))))
	(setq sql-global-history (cons sql-last-command sql-global-history)))
    (setq sql-global-history-index 0)))

(defun sql-previous-global-history ()
  "Copy the previous global history element into the current buffer."
  (interactive)
  (sql-goto-batch-buffer)
  (if (equal sql-global-history-index (length sql-global-history))
      (error "You have reached the beginning of the global history list."))
  (setq sql-global-history-index (1+ sql-global-history-index))
  (erase-buffer)
  (insert (nth (1- sql-global-history-index) sql-global-history))
;  (if (buffer-live-p sql-matching-buffer)
;      (progn
;	(set-buffer sql-matching-buffer)
;	(erase-buffer)
;	(set-buffer-modified-p nil)
;	(set-buffer sql-matching-buffer)))
  (set-buffer-modified-p nil)
  (message "Global history element: %d (unlinked)" sql-global-history-index))

(defun sql-next-global-history ()
  "Copy the next global history element into the current buffer."
  (interactive)
  (sql-goto-batch-buffer)
  (if (equal sql-global-history-index 0)
      (error "You have reached the end of the global history list."))
  (setq sql-global-history-index (1- sql-global-history-index))
  (erase-buffer)
  (if (equal sql-global-history-index 0)
      ()
    (insert (nth (1- sql-global-history-index) sql-global-history)))
;  (if (buffer-live-p sql-matching-buffer)
;      (progn
;	(set-buffer sql-matching-buffer)
;	(erase-buffer)
;	(set-buffer-modified-p nil)
;	(set-buffer sql-matching-buffer)))
  (set-buffer-modified-p nil)
  (message "Global history element: %d (unlinked)" sql-global-history-index))

(defun sql-previous-matching-history (pattern arg)
  "Copy the previous history element matching regexp PATTERN into the buffer.
With prefix ARG, keep the results buffer as-in (unlinked)."
  (interactive "sGoto history matching: 
P")
  (sql-goto-batch-buffer)
  (let ((history-part (nthcdr sql-history-index sql-history))
	(index 1)
	(found nil))
    (while (and history-part (not found))
      (if (string-match pattern (car (car history-part)))
	  (setq found t)
	(setq history-part (cdr history-part))
	(setq index (1+ index))))
    (if found
	(sql-goto-history (+ index sql-history-index) arg)
      (error "History matching '%s' not found." pattern))))

(defun sql-insert-in-results-buffer (text)
  "Insert TEXT in to the matching results buffer."
  (if (buffer-live-p sql-matching-buffer)
      (progn
	(set-buffer sql-matching-buffer)
	(erase-buffer)
	(insert text)
	(setq sql-current-error-point nil)
	(if (sql-multi-headers)
	    (setq sql-move-headers nil)
	  (setq sql-move-headers t)
	  (setq sql-header-text
		(buffer-substring (point-min)
				  (save-excursion
				    (goto-char (point-min))
				    (forward-line 2) (point)))))
	(set-buffer-modified-p nil)
	(set-buffer sql-matching-buffer))))

(defun sql-previous-history (arg)
  "Copy the previous history element into the current buffer.
With prefix arg, don't update the results buffer"
  (interactive "P")
  (sql-goto-batch-buffer)
  (setq sql-linked "(linked)")
  (if (equal sql-history-index 0)
      (setq sql-end-history (buffer-substring (point-min) (point-max))))
  (if (equal sql-history-index (length sql-history))
      (error "You have reached the beginning of the history list."))
  (sql-set-history-index (1+ sql-history-index))
  (erase-buffer)
  (insert (car (nth (1- sql-history-index) sql-history)))
  (if arg
      (setq sql-linked "(unlinked)")
    (sql-insert-in-results-buffer (cdr (nth (1- sql-history-index)
					    sql-history))))
  (set-buffer-modified-p nil)
  (message "History element: %d %s" sql-history-index sql-linked))

(defun sql-next-history (arg)
  "Copy the next history element into the current buffer.
With prefix arg, don't update the results buffer"
  (interactive "P")
  (sql-goto-batch-buffer)
  (setq sql-linked "(linked)")
  (if (equal sql-history-index 0)
      (error "You have reached the end of the history list.")
    (if (equal sql-history-index 1)
	(progn
	  (sql-set-history-index (1- sql-history-index))
	  (erase-buffer)
	  (insert sql-end-history)
	  (if arg
	      (setq sql-linked "(unlinked)")
	    (sql-insert-in-results-buffer ""))
	  (set-buffer-modified-p nil))
      (sql-set-history-index (1- sql-history-index))
      (erase-buffer)
      (if (not (equal sql-history-index 0))
	  (insert (car (nth (1- sql-history-index) sql-history))))
      (if arg
	  (setq sql-linked "(unlinked)")
	(sql-insert-in-results-buffer (cdr (nth (1- sql-history-index)
						sql-history)))))
    (set-buffer-modified-p nil)
    (message "History element: %d %s" sql-history-index sql-linked)))

(defun sql-goto-history (history-number arg)
  "Insert the history element HISTORY-NUMBER into the current buffer.
With prefix ARG, leave the results buffer as-is (unlinked)."
  (interactive "nHistory Number: 
p")
  (sql-goto-batch-buffer)
  (setq sql-linked "(linked)")
  (if (or (< history-number 0) (> history-number (length sql-history)))
      (error "%d is out of range.  Valid numbers are 0 - %d."
	     history-number (length sql-history)))
  (sql-set-history-index history-number)
  (if (equal sql-history-index 0)
      (setq sql-end-history (buffer-substring (point-min) (point-max))))
  (erase-buffer)
  (if (> history-number 0)
      (progn
	(insert (car (nth (1- sql-history-index) sql-history)))
	(if arg
	    (setq sql-linked "(unlinked)")
	  (sql-insert-in-results-buffer (cdr (nth (1- sql-history-index)
						  sql-history)))))
    (if arg
	(setq sql-linked "(unlinked)")
      (sql-insert-in-results-buffer "")))
  (set-buffer-modified-p nil)
  (message "History element: %d %s" sql-history-index sql-linked))

(defun sql-goto-global-history (history-number)
  "Insert the global history element HISTORY-NUMBER into the current buffer."
  (interactive "nGlobal History Number: ")
  (sql-goto-batch-buffer)
  (if (null sql-global-history)
      (error "The global history list is empty."))
  (if (or (< history-number 0) (> history-number (length sql-global-history)))
      (error "%d is out of range.  Valid numbers are 0 - %d."
	     history-number (length sql-global-history)))
  (setq sql-global-history-index history-number)
  (erase-buffer)
  (and (> history-number 0)
       (insert (nth sql-global-history-index sql-global-history)))
  (sql-insert-in-results-buffer "")
  (set-buffer-modified-p nil)
  (message "Global history element: %d (unlinked)" sql-global-history-index))

(defun sql-save-global-history ()
  "Save the current global history to disk.
Use the file specified by `sql-global-history-file-name'."
  (interactive)
  (if sql-global-history
      (let ((buf (generate-new-buffer " global-history-temp"))
	    (history sql-global-history)
	    (old-standard-output standard-output)
	    (old-buffer (current-buffer)))
	(setq standard-output buf)
	(set-buffer buf)
	(princ "(setq sql-global-history\n      '")
	(prin1 history)
	(princ ")\n")
	(setq standard-output old-standard-output)
	(write-file sql-global-history-file-name)
	(set-buffer old-buffer)
	(kill-buffer buf))
    (error "There is no global history currently available.")))

(defun sql-set-history-index (index)
  "Set the variable sql-history-index to INDEX."
  (setq sql-history-index index)
  (and sql-history-in-modeline
       (setq sql-history-index-string
	     (concat "H:" (int-to-string index)
		     "/" (int-to-string (length sql-history))))))

(defun sql-maybe-delete-frame (&optional frame)
  (interactive)
  (and frame (select-frame frame))
  (cond ((eq major-mode 'sql-batch-mode)
	 (and sql-batch-in-new-frame
	      (eq (selected-frame) sql-batch-frame)
	      (delete-frame)))
	((eq major-mode 'sql-interactive-mode)
	 (and sql-interactive-in-new-frame
	      (eq (selected-frame) sql-interactive-frame)
	      (delete-frame)))
	((eq major-mode 'sql-results-mode)
	 (and sql-results-in-new-frame
	      (eq (selected-frame) sql-results-frame)
	      (delete-frame)))))

(defun sql-save-cache-data-all-buffers ()
  "Save the current cached information to disk for all sql-batch-mode buffers.
Use the file specified by appending the current server and database of each
buffer to the value of the variable `sql-cache-data-file-name'."
  (interactive)
  (mapcar 'sql-save-cache-data (buffer-list)))

(defun sql-save-history-all-buffers ()
  "Save the current history information to disk for all sql-batch-mode buffers.
Use the file specified by appending the current server and database of each
buffer to the value of the variable `sql-history-file-name'."
  (interactive)
  (mapcar 'sql-save-history (buffer-list)))

(defun sql-save-cache-data (&optional buffer)
  "Save the current cached information to disk for the current buffer.
Use the file specified by appending the current server and database
to the value of the variable `sql-cache-data-file-name'."
  (interactive)
  (and buffer (set-buffer buffer))
  (if (not (eq major-mode 'sql-batch-mode))
      (if (interactive-p)
	  (error "You must be in a sql-batch-mode buffer.")
	())
    (if (not sql-modified-cache)
	(if (interactive-p)
	    (message "(No changes need to be saved)")
	  ())
      (message "Saving cache data for %s..." sql-server)
      (let* ((old-buffer (current-buffer))
	     (cache-file (concat sql-cache-data-file-name "-" sql-server "-"
				 sql-database))
	     (cache-buffer (find-file-noselect cache-file))
	     (tables sql-table-list)
	     (columns sql-column-list)
	     (values sql-value-list)
	     (stored-procedures sql-stored-procedure-list)
	     (users sql-user-list)
	     (databases sql-database-list)
	     (old-standard-output standard-output))
	(set-buffer cache-buffer)
	(setq standard-output cache-buffer)
	(erase-buffer)
	(insert "(setq sql-table-list\n      '")
	(prin1 tables)
	(insert ")\n\n")
	(insert "(setq sql-column-list\n      '")
	(prin1 columns)
	(insert ")\n\n")
	(insert "(setq sql-value-list\n      '")
	(prin1 values)
	(insert ")\n\n")
	(insert "(setq sql-stored-procedure-list\n      '")
	(prin1 stored-procedures)
	(insert ")\n\n")
	(insert "(setq sql-database-list\n      '")
	(prin1 databases)
	(insert ")\n\n")
	(insert "(setq sql-user-list\n      '")
	(prin1 users)
	(insert ")\n\n")
	(or (file-directory-p (concat (getenv "HOME") "/.sql-cache-dir"))
	    (make-directory (concat (getenv "HOME") "/.sql-cache-dir")))
	(save-buffer)
	(set-buffer old-buffer)
	(setq standard-output old-standard-output)
	(setq sql-modified-cache nil)
	(message "Saving cache data for %s... done" sql-server)))))

(defun sql-save-history (&optional buffer)
  "Save the current history to disk for the current buffer.
Use the file specified by appending the current server and database
to the value of the variable `sql-history-file-name'."
  (interactive)
  (and buffer (set-buffer buffer))
  (if (not (eq major-mode 'sql-batch-mode))
      (if (interactive-p)
	  (error "You must be in a sql-batch-mode buffer.")
	())
    (if (not sql-modified-history)
	(if (interactive-p)
	    (message "(No changes need to be saved)")
	  ())
      (message "Saving history for %s..." sql-server)
      (let* ((old-buffer (current-buffer))
	     (history-file (concat sql-history-file-name "-" sql-server "-"
				   sql-database))
	     (history-buffer (find-file-noselect history-file))
	     (history sql-history)
	     (old-standard-output standard-output))
	(set-buffer history-buffer)
	(setq standard-output history-buffer)
	(erase-buffer)
	(insert "(setq sql-history\n      '")
	(prin1 history)
	(insert ")\n\n")
	(or (file-directory-p (concat (getenv "HOME") "/.sql-history-dir"))
	    (make-directory (concat (getenv "HOME") "/.sql-history-dir")))
	(save-buffer)
	(set-buffer old-buffer)
	(setq standard-output old-standard-output)
	(setq sql-modified-history nil)
	(message "Saving history for %s... done" sql-server)))))

(defun sql-load-history ()
  "Load saved history information from disk.
Use the file specified by appending the current server and database
to the value of the variable `sql-history-file-name'."
  (interactive)
  (let ((history-file (concat sql-history-file-name "-" sql-server "-" sql-database)))
    (if (not (file-exists-p history-file))
	(if (interactive-p)
	    (error "There is no saved history file for this server and database")
	  ())
      (or (file-readable-p history-file)
	  (error "Problems reading file %s" history-file))
      (load-file history-file)
      (setq sql-modified-history nil))))

(defun sql-load-cache-data ()
  "Load cached information from disk.
Use the file specified by appending the current server and database
to the value of the variable `sql-cache-data-file-name'."
  (interactive)
  (let* ((cache-file (concat sql-cache-data-file-name "-" sql-server "-" sql-database)))
    (if (not (file-exists-p cache-file))
	(if (interactive-p)
	    (error "There is no saved cache file for this server and database")
	  ())
      (or (file-readable-p cache-file)
	  (error "Problems reading file %s" cache-file))
      (load-file cache-file)
      (setq sql-modified-cache nil))))
  
(defun sql-save-top-ten ()
  "Save the current top ten list to disk.
Use the file specified by `sql-top-ten-file-name'."
  (interactive)
  (if sql-top-ten
      (let ((buf (generate-new-buffer " top-ten-temp"))
	    (top-ten sql-top-ten)
	    (top-ten-aliases sql-top-ten-aliases)
	    (top-ten-points sql-top-ten-points)
	    (top-ten-execute sql-top-ten-execute)
	    (top-ten-help sql-top-ten-help)
	    (old-standard-output standard-output)
	    (old-buffer (current-buffer)))
	(setq standard-output buf)
	(set-buffer buf)
	(princ "(setq sql-top-ten\n      '")
	(prin1 top-ten)
	(princ ")\n")
	(princ "(setq sql-top-ten-aliases\n      '")
	(prin1 top-ten-aliases)
	(princ ")\n")
	(princ "(setq sql-top-ten-points\n      '")
	(prin1 top-ten-points)
	(princ ")\n")
	(princ "(setq sql-top-ten-execute\n      '")
	(prin1 top-ten-execute)
	(princ ")\n")
	(princ "(setq sql-top-ten-help\n      '")
	(prin1 top-ten-help)
	(princ ")\n")
	(setq standard-output old-standard-output)
	(write-file sql-top-ten-file-name)
	(set-buffer old-buffer)
	(kill-buffer buf))
    (error "There is no top ten list currently available.")))

(defun sql-load-global-history ()
  "Load the history file $HOME/.sql-global-history"
  (if (file-readable-p sql-global-history-file-name)
      (progn
	(load-file sql-global-history-file-name)
	(setq sql-global-history-index 0))
    (error "Problems reading file %s" sql-global-history-file-name)))
  
(defun sql-load-top-ten ()
  "Load the top-ten file $HOME/.sql-top-ten"
  (interactive)
  (if (file-readable-p sql-top-ten-file-name)
      (progn
	(load-file sql-top-ten-file-name)
	(setq sql-use-top-ten-toolbar sql-xemacs-19-12))
    (setq sql-use-top-ten-toolbar nil)
    (and (interactive-p)
	 (error "Problems reading file %s" sql-top-ten-file-name))))

(defun sql-add-top-ten (index mnemonic)
  "Add the current buffer to the top ten list."
  (interactive "nTop Ten index (0-9): 
sMnemonic for this Top Ten (RET for no mnemonic): ")
  (let ((execute (y-or-n-p "Should this Top Ten item be executed immediately following insertion? ")))
    (if (or (> index 9) (< index 0))
	(error "%d is not in the range 0-9." index)
      (aset sql-top-ten index (buffer-substring (point-min) (point-max)))
      (aset sql-top-ten-aliases index mnemonic)
      (aset sql-top-ten-points index (point))
      (aset sql-top-ten-execute index execute)
      (aset sql-top-ten-help index
	    (let ((len (length (aref sql-top-ten-aliases index)))
		  (str (buffer-substring (point-min)
					 (save-excursion
					   (goto-char (point-min))
					   (end-of-line)
					   (point)))))
	      (concat
	       (if (aref sql-top-ten-execute index)
		   "Execute the Top Ten Item "
		 "Insert the Top Ten Item ")
	       (if (> len 0)
		   (aref sql-top-ten-aliases index)
		 (concat "#" index))
	       (if (or (> (length str) (- 45 len))
		       (> (length (aref sql-top-ten index))
			  (1+ (length str))))
		   (concat " ("
			   (substring str 0 (min (length str) (- 45 len)))
			   "...)")
		 (concat " (" str ")")))))
      (sql-save-top-ten))))

(defun sql-insert-top-ten-0 ()
  "Insert a commonly used query into the buffer."
  (interactive)
  (sql-insert-top-ten "0"))

(defun sql-insert-top-ten-1 ()
  "Insert a commonly used query into the buffer."
  (interactive)
  (sql-insert-top-ten "1"))

(defun sql-insert-top-ten-2 ()
  "Insert a commonly used query into the buffer."
  (interactive)
  (sql-insert-top-ten "2"))

(defun sql-insert-top-ten-3 ()
  "Insert a commonly used query into the buffer."
  (interactive)
  (sql-insert-top-ten "3"))

(defun sql-insert-top-ten-4 ()
  "Insert a commonly used query into the buffer."
  (interactive)
  (sql-insert-top-ten "4"))

(defun sql-insert-top-ten-5 ()
  "Insert a commonly used query into the buffer."
  (interactive)
  (sql-insert-top-ten "5"))

(defun sql-insert-top-ten-6 ()
  "Insert a commonly used query into the buffer."
  (interactive)
  (sql-insert-top-ten "6"))

(defun sql-insert-top-ten-7 ()
  "Insert a commonly used query into the buffer."
  (interactive)
  (sql-insert-top-ten "7"))

(defun sql-insert-top-ten-8 ()
  "Insert a commonly used query into the buffer."
  (interactive)
  (sql-insert-top-ten "8"))

(defun sql-insert-top-ten-9 ()
  "Insert a commonly used query into the buffer."
  (interactive)
  (sql-insert-top-ten "9"))

(defun sql-run-top-ten-0 ()
  "Insert a commonly used query into the buffer."
  (interactive)
  (sql-insert-top-ten "0")
  (sql-evaluate-buffer nil))

(defun sql-run-top-ten-1 ()
  "Insert a commonly used query into the buffer."
  (interactive)
  (sql-insert-top-ten "1")
  (sql-evaluate-buffer nil))

(defun sql-run-top-ten-2 ()
  "Insert a commonly used query into the buffer."
  (interactive)
  (sql-insert-top-ten "2")
  (sql-evaluate-buffer nil))

(defun sql-run-top-ten-3 ()
  "Insert a commonly used query into the buffer."
  (interactive)
  (sql-insert-top-ten "3")
  (sql-evaluate-buffer nil))

(defun sql-run-top-ten-4 ()
  "Insert a commonly used query into the buffer."
  (interactive)
  (sql-insert-top-ten "4")
  (sql-evaluate-buffer nil))

(defun sql-run-top-ten-5 ()
  "Insert a commonly used query into the buffer."
  (interactive)
  (sql-insert-top-ten "5")
  (sql-evaluate-buffer nil))

(defun sql-run-top-ten-6 ()
  "Insert a commonly used query into the buffer."
  (interactive)
  (sql-insert-top-ten "6")
  (sql-evaluate-buffer nil))

(defun sql-run-top-ten-7 ()
  "Insert a commonly used query into the buffer."
  (interactive)
  (sql-insert-top-ten "7")
  (sql-evaluate-buffer nil))

(defun sql-run-top-ten-8 ()
  "Insert a commonly used query into the buffer."
  (interactive)
  (sql-insert-top-ten "8")
  (sql-evaluate-buffer nil))

(defun sql-run-top-ten-9 ()
  "Insert a commonly used query into the buffer."
  (interactive)
  (sql-insert-top-ten "9")
  (sql-evaluate-buffer nil))

(defun sql-vector-element-index (vector element)
  (let ((counter 0)
	(length (length vector))
	(result nil))
    (while (< counter length)
      (if (equal (aref vector counter) element)
	  (progn
	    
	    (setq result counter)
	    (setq counter length))
	(setq counter (1+ counter))))
    result))      

(defun sql-insert-top-ten (index)
  (interactive "sTop ten index or mnemonic: ")
  (sql-goto-batch-buffer)
  (let ((old-index index))
    (and (sql-vector-element-index sql-top-ten-aliases index)
	 (setq index (sql-vector-element-index sql-top-ten-aliases index)))
    (and (stringp index)
	 (sql-string-is-numberp index)
	 (setq index (string-to-int index)))
    (let ((query (aref sql-top-ten index))
	  (location (aref sql-top-ten-points index))
	  (execute (aref sql-top-ten-execute index)))
;	  (this-buffer (current-buffer)))
      (if (not query)
	  (error "Can't find Top Ten entry for %s" old-index)
	(erase-buffer)
	(insert query)
	(and location (goto-char location))
	(and execute (sql-evaluate-buffer nil))
;      (if (sql-goto-matching-buffer t)
;	  (erase-buffer))
;      (set-buffer this-buffer)
	(message "Top Ten %s." old-index)))))

(defun sql-check-for-where ()
  "Signal an error if no `where' clause is found in the current buffer.
Check for the occurance of `sql-require-where-regexp' and signal error
if there is no `where' clause."
  (and sql-require-where
       (sql-search-for-regexp sql-require-where-regexp nil)
       (not (sql-search-for-regexp "[\t\n ]where[\t\n ]" t))
       (not (sql-popup-dialog "WARNING: There is no `where' clause in this statement.  Execute anyway?


(See the help on the variable `sql-require-where' to supress this warning)"))
       (error "Aborted.")))

(defun sql-popup-dialog (prompt)
  (if sql-lucid
      (sql-popup-dialog-return (list prompt ["YES" t t] nil ["NO" 'no t]))
    (x-popup-dialog t (cons prompt '(("YES" . t) ("NO" . nil))))))

(defun sql-popup-dialog-return (dbox-desc)
  "Ask user a question with a popup dialog box.
Takes one argument, which is the dialog box description (see the function
popup-dialog-box).

Returns the callback specified by DBOX-DESC."
  (let ((echo-keystrokes 0)
	event)	 
    (popup-dialog-box dbox-desc)
    (let ((result (catch 'dialog-done
		    (while t
		      (setq event (next-command-event event))
		      (cond ((and (menu-event-p event)
				  (or (eq (event-object event) 'abort)
				      (eq (event-object event)
					  'menu-no-selection-hook)))
			     (signal 'quit nil))
			    ((menu-event-p event)
			     (throw 'dialog-done (event-object event)))
			    ((button-release-event-p event) ;; don't beep twice
			     nil)
			    (t
			     (beep)
			     (message "Please answer the dialog box")))))))
      (if (equal result '(quote no))
	  nil
	result))))

(defun sql-get-rows-affected (flag)
  "Check how many rows would be affected if current buffer was evaluated."
  (if (not (sql-search-for-regexp sql-confirm-changes-regexp nil))
      t
    (let* ((the-old-window (selected-window))
	   (the-old-buffer (current-buffer))
	   (the-old-point (point))
	   (beg (if flag (min (point) (mark)) (point-min)))
	   (end (if flag (max (point) (mark)) (point-max)))
	   (command (buffer-substring beg end))
	   (temp-buffer (get-buffer-create " SQL-TEMP"))
	   (sql-getting-completions t)
	   (sql-completion-buffer temp-buffer)
	   (sql-evaluation-method 'foreground)
	   (rows nil))
      (set-buffer temp-buffer)
      (erase-buffer)
      (insert command)
      (goto-char (point-max))
      (and sql-require-final-go (insert "\ngo\n"))
      (goto-char (point-min))
      (insert (if sql-database (concat "use " sql-database "\ngo\n") ""))
      (insert "begin tran\n")
      (while (re-search-forward "^go\\>" nil t)
	(beginning-of-line)
	(insert "rollback tran\n")
	(end-of-line)
	(insert "\nbegin tran\n"))
      (backward-delete-char 12)
      (setq command (buffer-substring (point-min) (point-max)))
      (erase-buffer)
      (set-buffer the-old-buffer)
      (setq sql-old-point (point))
      (sql-evaluate command sql-server sql-user sql-password
		    sql-batch-command-switches sql-matching-buffer t)
      (set-buffer temp-buffer)
      (goto-char (point-min))
      (while (re-search-forward "[0-9]+> " nil t)
	(replace-match "" nil nil))
      (setq rows (buffer-substring (point-min) (point-max)))
      (and (> (length rows) 500)
	   (setq rows (substring rows 0 500)))
      (kill-buffer temp-buffer)
      (set-buffer the-old-buffer)
      (select-window the-old-window)
      (goto-char the-old-point)
      (sql-popup-dialog (concat sql-warning-prefix rows sql-warning-suffix)))))

(defun sql-search-for-regexp (regexp check-syntax)
  "Search for REGEXP in non-commented text in current buffer.
Search for REGEXP in the current buffer, ignoring it if it is within
a comment block and CHECK-SYNTAX is non-nil."
  (save-excursion
    (goto-char (point-min))
    (let ((found nil))
      (while (re-search-forward regexp nil t)
	(if (and sql-lucid
		 (or check-syntax sql-risky-searches)
		 (or (eq (buffer-syntactic-context) 'comment)
		     (eq (buffer-syntactic-context) 'block-comment)))
	    nil
	  (setq found t)))
      found)))

(defun sql-set-server ()
  "Change the server, user, and password in the current sql-mode buffer."
  (interactive)
  (let* ((new-server (completing-read "Server Name: " sql-server-table))
	 (new-user (completing-read (format "User Name on Server %s: "
					    new-server)
				    sql-user-table))
	 (new-password (let ((prompt (format "Password for %s on %s: " 
					     new-user new-server)))
			 (if sql-secure-passwords
			     (sql-read-password prompt)
			   (read-string prompt)))))
    (sql-batch-mode new-server new-user new-password nil t)))

(defun sql-set-user ()
  "Change the user and password in the current sql-mode buffer."
  (interactive)
  (let* ((new-user (completing-read "User Name: " sql-user-table))
	 (new-password (let ((prompt (format "Password for %s on %s: " 
					     new-user sql-server)))
			 (if sql-secure-passwords
			     (sql-read-password prompt)
			   (read-string prompt)))))
    (sql-batch-mode sql-server new-user new-password nil t)))

(defun sql-set-password ()
  "Change the password in the current sql-mode buffer."
  (interactive)
  (let ((new-password (let ((prompt (format "Password for %s on %s: " 
					    sql-user sql-server)))
			(if sql-secure-passwords
			    (sql-read-password prompt)
			  (read-string prompt)))))
    (sql-batch-mode sql-server sql-user new-password nil t)))

(defun sql-set-bcp-user (user)
  "Change the name of the bcp user."
  (interactive (list (read-from-minibuffer (format "New BCP user (was %s): "
						   (or sql-bcp-user "nil")))))
  (if (string-equal user "")
      (setq sql-bcp-user nil)
    (setq sql-bcp-user user)))

(defun sql-mode-set-association (mnemonic)
  "Set an association for the current sql-mode buffer."
  (interactive (list (completing-read "Association mnemonic: "
				      (delete nil
					      (mapcar
					       'sql-remove-dashes
					       sql-association-alist)))))
  (or (eq major-mode 'sql-mode)
      (error "You must be in a sql-mode buffer."))
  (make-variable-buffer-local 'sql-evaluation-buffer)
  (let* ((association (sql-get-association mnemonic sql-association-alist))
	 (server (nth 0 association))
	 (user (nth 1 association))
	 (password (nth 2 association))
	 (database (nth 3 association)))
    (sql-mode-set-evaluation-buffer server user password database)))

(defun sql-mode-set-evaluation-buffer (&optional server user password database)
  (interactive)
  (setq server (or server (completing-read "Server Name: " sql-server-table)))
  (setq user (or user (completing-read (format "User Name on Server %s: " 
					       server)
				       sql-user-table)))
  (setq password (or password
		     (let ((prompt (format "Password for %s on %s: "
					   user server)))
		       (if sql-secure-passwords
			   (sql-read-password prompt)
			 (read-string prompt)))))
  (let* ((name (concat "+" server "+" user "+"
		       (if database
			   (concat database "+")
			 "")))
	 (existing-buffer (buffer-live-p (get-buffer name))))
    (if existing-buffer
	(setq sql-evaluation-buffer (get-buffer name))
      (let ((this-frame (selected-frame)))
	(select-frame (make-frame))
	(sql-batch-mode server user password database)
	(select-frame this-frame)
	;; now the buffer should exist
	(setq sql-evaluation-buffer (get-buffer name))))))

(defun sql-mode-evaluate (string)
  (or (buffer-live-p sql-evaluation-buffer)
      (error "No batch buffer associated with this one."))
  (let ((this-window (selected-window)))
    (set-buffer sql-evaluation-buffer)
    (erase-buffer)
    (insert string)
    (select-window (car (windows-of-buffer (current-buffer))))
    (sql-evaluate-buffer nil)
    (select-window this-window)))

(defun sql-mode-evaluate-buffer ()
  "Send the contents of the current buffer to the associated sql-batch-mode
buffer."
  (interactive)
  (sql-mode-evaluate (buffer-substring (point-min) (point-max))))

(defun sql-mode-evaluate-region (beg end)
  "Send the contents of the region to the associated sql-batch-mode buffer."
  (interactive "r")
  (sql-mode-evaluate (buffer-substring beg end)))

(defun sql-mode-evaluate-statement ()
  "Send the code that makes up a statement around point to the associated
sql-batch-mode buffer."
  (interactive)
  (let (beg end)
    (save-excursion
      (and (> (point) (point-min)) (forward-char -1))
      (setq beg (save-excursion (if (re-search-backward "^go\\>" nil t)
				    (+ (point) 3)
				  (point-min))))
      (setq end (save-excursion (if (re-search-forward "^go\\>" nil t)
				    (point)
				  (point-max)))))
    (sql-mode-evaluate (buffer-substring beg end))))

(defun sql-add-association (add-mnemonic 
			    add-server 
			    add-user 
			    &optional add-password
			    add-database)
  "Add the association referenced by MNEMONIC.
Additions are made to the variable `sql-association-alist.'"
  (interactive "sMnemonic: 
sServer Name: 
sUser Name: ")
  (if (sql-get-association add-mnemonic sql-association-alist)
      (if (interactive-p)
	  (error "There already is an association for mnemonic: %s" 
		 add-mnemonic))
    (if (null add-password)
	(progn
	  (setq new-password (let ((prompt "Password: "))
			       (if sql-secure-passwords
				   (sql-read-password prompt)
				 (read-string prompt))))
	  (sit-for 0)))
    (setq sql-association-alist
	  (cons (list add-mnemonic (list add-server add-user
					 add-password add-database))
		sql-association-alist))
    (setq sql-server-table (cons (cons add-server "1") sql-server-table))
    (setq sql-user-table (cons (cons add-user "1") sql-user-table))))

(defun sql-get-association (mnemonic alist)
  "Return the association referenced by MNEMONIC."
  (if (string-equal mnemonic "-")
      nil
    (if (null alist) 
	nil
      (if (string-equal mnemonic (car (car alist)))
	  (car (cdr (car alist)))
	(sql-get-association mnemonic (cdr alist))))))

(defun sql-set-tables ()
  "Set the sql-server-table and sql-user-table variables."
  (let ((the-list sql-association-alist))
    (while the-list
      (if (string-equal (car (car the-list)) "-")
	  ()
	(setq the-server (car (car (cdr (car the-list)))))
	(setq the-user (car (cdr (car (cdr (car the-list))))))
	(setq sql-server-table (cons (cons the-server 1) sql-server-table))
	(setq sql-user-table (cons (cons the-user 1) sql-user-table)))
      (setq the-list (cdr the-list)))))

(defun sql-load-customizations ()
  "Load the file $HOME/.sql-mode"
  (let ((sql-file-name (concat (getenv "HOME") "/.sql-mode")))
    (if (file-readable-p sql-file-name)
	(progn
	  (load-file sql-file-name)
	  (sql-set-tables)))))

(defun sql-remove-dashes (elt)
  (if (string-equal (car elt) "-")
      nil
    elt))
      
(defun sql-association-mode (mnemonic &optional no-create interactive)
  "Invoke an sql-batch-mode buffer with information specified by MNEMONIC.
The server, user, and password are determined by referencing MNEMONIC in
`sql-association-alist'.  See sql-add-association for more detail on
mnemonics.  Invoke an sql-interactive-mode buffer if optional INTERACTIVE
is non-nil."
  (interactive (list (completing-read "Association mnemonic: "
				      (delete nil
					      (mapcar
					       'sql-remove-dashes
					       sql-association-alist)))))
  (if (and no-create
	   (not (eq major-mode 'sql-batch-mode))
	   (not (eq major-mode 'sql-interactive-mode)))
      (setq no-create nil))
  (let ((association (sql-get-association mnemonic sql-association-alist)))
    (if association
	(if interactive
	    (sql-interactive-mode (nth 0 association) 
				  (nth 1 association) 
				  (nth 2 association))
	  (sql-batch-mode (nth 0 association) 
			  (nth 1 association) 
			  (nth 2 association)
			  (nth 3 association)
			  no-create))
      (error "There is no association for mnemonic %s" mnemonic))))

(defun sql-make-association-menu (assoc-alist mode)
  "Return a list of all server/login-id pairs in menu format."
  (if (null assoc-alist)
      nil
    (let* ((head (car assoc-alist))
	  (tail (cdr assoc-alist))
	  (popup-item (if (string-equal (nth 0 head) "-")
			  (nth 0 (car (cdr head)))
			(vector
			 (or (nth 2 head)
			     (concat (nth 0 head) "  "
				     (nth 0 (car (cdr head))) "  "
				     (nth 1 (car (cdr head))) "  "
				     (nth 3 (car (cdr head)))))
			 (list 'sql-association-mode 
			       (nth 0 head)
			       sql-association-mode-no-create
			       (if (eq mode 'sql-interactive-mode)
				   t
				 nil))
			 't))))
      (cons popup-item (sql-make-association-menu tail mode)))))

(defun sql-make-popup-menu (mode)
  "Return a menu of all defined associations."
  (cond
   ((eq mode 'sql-batch-mode)
    (cons "SQL Batch Associations"
	  (sql-make-association-menu sql-association-alist mode)))
   ((eq mode 'sql-interactive-mode)
    (cons "SQL Interactive Associations"
	  (sql-make-association-menu sql-association-alist mode)))
   (t
    nil)))

(defun sql-popup-association-menu ()
  "Pop up a menu of all defined associations.
Associations are stored in the variable `sql-association-alist'."
  (interactive)
  (if (null sql-association-alist)
      nil
    (cond 
     ((eq major-mode 'sql-batch-mode)
      (popup-menu (sql-make-popup-menu 'sql-batch-mode)))
     ((eq major-mode 'sql-interactive-mode)
      (popup-menu (sql-make-popup-menu 'sql-interactive-mode)))
     (t
      (error "You must be in a sql-batch-mode or a sql-interactive-mode buffer.")))))

(defun sql-replace-word (string)
  "Replace the current word with STRING."
  (interactive)
  (or (char-equal (preceding-char) ? )
      (backward-kill-word 1))
  (or (char-equal (following-char) ? )
      (kill-word 1))
  (insert string)
  (if (looking-at " ")
      (forward-char 1)
    (insert " ")))

(defun sql-split (list n)
  (let ((remain list)
        (result '())
        (sublist '())
        (i 0))
    (while remain
      (or (string-equal (car (car remain)) "
")
	  (setq sublist (cons (car remain) sublist)))
      (setq remain (cdr remain))
      (setq i (1+ i))
      (and (= i n)
           ;; We have finished a sublist
           (progn (setq result (cons sublist result))
                  (setq i 0)
                  (setq sublist '()))))
    ;; There might be a sublist (if the length of LIST mod n is != 0)
    ;; that has to be added to the result list.
    (and sublist
         (setq result (cons sublist result)))
    result))

(defun sql-index-sublist-1 (sublist ix limit)
  (let ((s1 (substring (car (car sublist)) 0 (min limit ix)))
        (s2 (substring
             (car (nth (1- (length sublist)) sublist))
             0 (min (length (car (nth (1- (length sublist)) sublist))) ix))))
    (cons s1 s2)))

(defun sql-index-sublist (sublist &rest count)
  (let* ((cmplength 100)
         (limit (length (car (car sublist))))
         (result (sql-index-sublist-1 sublist cmplength limit))
         (str1 (car result))
         (str2 (cdr result)))
    (while (and (string-equal str1 str2) (< cmplength limit))
      (setq cmplength (1+ cmplength)
            result (sql-index-sublist-1 sublist cmplength limit)
            str1 (car result)
            str2 (cdr result)))
    (cond ((not (string-equal str1 str2))
           (format "%s ... %s" str1 str2))
          ((< cmplength limit)
           (format "%s" str1))
          (t
           (format "%s ..." str1)))))

(defun sql-popup-continuation-menu (menu title)
  "Pop up a menu with more... entries if overflow."
  (interactive)
  (let* ((count 0)
	 (split-menu
	  (mapcar
	   (function
	    (lambda (sublist)
	      (setq count (1+ count))
	      (cons (format "%s" (sql-index-sublist sublist count))
		    (mapcar
		     (function
		      (lambda (menu)
			(vector (format "%s" (car menu))
				(list 'sql-replace-word
				      (car menu))
				t)))
		     sublist))))
	   (sql-split menu 20))))
    (popup-menu (cons title split-menu))))

(defun sql-popup-table-list ()
  "Pop up a menu displaying the tables to choose from."
  (interactive)
  (or sql-table-list
      (sql-get-tables))
  (sql-popup-continuation-menu sql-table-list
			       (concat "Tables" (or sql-database ""))))

(defun sql-popup-column-list ()
  "Pop up a menu displaying the columns to choose from."
  (interactive)
  (let* ((alias (sql-get-column-alias))
	 (table-name (sql-get-table-name alias)))
    (if (not (assoc table-name sql-column-list))
	(sql-get-columns table-name))
    (sql-popup-continuation-menu (cdr (assoc table-name sql-column-list))
				 (concat "Columns for " table-name))))
	  
(defun sql-popup-value-list ()
  "Pop up a menu displaying the values to choose from."
  (interactive)
  (let ((table-name (sql-get-table-name))
	(column-name (sql-get-column-name)))
    (if (not (assoc (cons table-name column-name) sql-value-list))
	(sql-get-values table-name column-name))
    (sql-popup-continuation-menu (cdr (assoc (cons table-name column-name)
					     sql-value-list))
				 (concat "Values for "
					 table-name
					 "."
					 column-name))))
	  
(defun sql-popup-stored-procedure-list ()
  "Pop up a menu displaying the stored-procedures to choose from."
  (interactive)
  (or sql-stored-procedure-list
      (sql-get-stored-procedures))
  (sql-popup-continuation-menu sql-stored-procedure-list "Stored Procedures"))

(defun sql-popup-user-list ()
  "Pop up a menu displaying the stored-procedures to choose from."
  (interactive)
  (or sql-user-list
      (sql-get-users))
  (sql-popup-continuation-menu sql-user-list "Users"))

(defun sql-popup-keyword-list ()
  "Pop up a menu displaying the stored-procedures to choose from."
  (interactive)
  (or sql-keyword-list
      (sql-get-keywords))
  (sql-popup-continuation-menu sql-keyword-list "Keywords"))

(defun sql-popup-operator-list ()
  "Pop up a menu displaying the stored-procedures to choose from."
  (interactive)
  (or sql-operator-list
      (sql-get-operators))
  (sql-popup-continuation-menu sql-operator-list "Operators"))

(defun sql-dynamic-popup-menu (event)
  "Pop up a menu depending on the context of point.
Possibilities include associations, or completion lists of tables, column,
stored procedures or keywords."
  (interactive "@e")
  (let ((pos (event-point event))
	(context))
    (save-excursion
      (goto-char (or pos (point-max)))
      (setq context (and pos (sql-get-completion-context)))
      (cond
       ((eq context 'table)
	(sql-popup-table-list))
       ((eq context 'column)
	(sql-popup-column-list))
       ((eq context 'stored-procedure)
	(sql-popup-stored-procedure-list))
       ((eq context 'keyword)
	(sql-popup-keyword-list))
       (t
	(sql-popup-association-menu))))
    (and pos (goto-char pos))))

(defun sql-load-abbrevs ()
  "Load any user-defined abbrevs for sql-mode."
  (let ((sql-abbrevs-file-name (concat (getenv "HOME") "/.sql-abbrevs")))
    (and (file-readable-p sql-abbrevs-file-name)
	 (read-abbrev-file sql-abbrevs-file-name))
    (setq local-abbrev-table sql-mode-abbrev-table)))

(defun sql-read-password (prompt &optional default)
  "Read a password from the user. Echos a * for each character typed.
End with RET, LFD, or ESC. DEL or C-h rubs out.  ^U kills line.
Optional DEFAULT is password to start with."
  (let ((pass (if default default ""))
	(c 0)
	(echo-keystrokes 0)
	(cursor-in-echo-area t))
    (while (and (/= c ?\r) (/= c ?\n) (/= c ?\e))
      (message "%s%s"
	       prompt
	       (make-string (length pass) ?*))
      (setq c (read-char))
      (if (= c ?\C-u)
	  (setq pass "")
	(if (and (/= c ?\b) (/= c ?\177))
	    (setq pass (concat pass (char-to-string c)))
	  (if (> (length pass) 0)
	      (setq pass (substring pass 0 -1))))))
    (message nil)
    (sit-for 0)
    (substring pass 0 -1)))

(setq sql-tmp-keymap (make-sparse-keymap))
(define-key sql-tmp-keymap "\C-m" 'exit-minibuffer)

; this function is going to be needed if sql-mode.el is ever ported to FSFmacs
;
;(defun sql-repaint-minibuffer ()
;  "Gross hack to set minibuf_message = 0, so that the contents of the
;minibuffer will show."
;  (if (eq (selected-window) (minibuffer-window))
;      (if (string-match "Lucid" emacs-version)
;	  (message nil)
;	;; v18 GNU Emacs
;	(let ((unread-command-char ?\C-m)
;	      (enable-recursive-minibuffers t))
;	  (read-from-minibuffer "" nil sql-tmp-keymap nil)))))

(defun sql-forward-column ()
  "Scroll the buffer one column to the left."
  (interactive)
  (scroll-left 1))

(defun sql-backward-column ()
  "Scroll the buffer one column to the right."
  (interactive)
  (scroll-right 1))

(defun sql-first-row ()
  "Warp point to the second line in the buffer (the first row)."
  (interactive)
  (setq zmacs-region-stays t)
  (goto-char (point-min))
  (forward-line 2))

(defun sql-last-row ()
  "Warp point to the last row in the buffer."
  (interactive)
  (setq zmacs-region-stays t)
  (goto-char (point-max))
  (forward-line -1))

(defun sql-beginning-of-row ()
  "Move point to beginning of current row."
  (interactive)
  (setq zmacs-region-stays t)
  (scroll-right sql-max-frame-width)
  (beginning-of-line))

(defun sql-end-of-row ()
  "Move point to end of current row."
  (interactive)
  (setq zmacs-region-stays t)
  (let ((end (progn (end-of-line) (current-column))))
    (sql-beginning-of-row)
    (scroll-left (- end (- (frame-width) sql-scroll-overlap)))
    (end-of-line)))

(defun sql-horizontal-recenter ()
  "Scroll the window as necessary horizontally so that point is centered."
  (interactive)
  (if (eq major-mode 'sql-results-mode)
      (sql-recenter t)))

(defun sql-reposition-windows ()
  "Resize the results buffer and display it."
  (interactive)
  (sql-goto-batch-buffer)
  (if (buffer-live-p sql-matching-buffer)
      (progn
	(sql-position-results-buffer sql-matching-buffer)
	(sql-goto-batch-buffer))
    (delete-other-windows)))

(defun sql-recenter (&optional horizontal-only)
  "Scroll the window as necessary so that point is centered.
Only scroll horizontally if optional HORIZONTAL-ONLY is non-nil."
  (interactive)
  (if (or (eq major-mode 'sql-results-mode)
	  (eq major-mode 'sql-interactive-mode))
      (let ((the-point (point)))
	(save-excursion
	  (let ((position (current-column))
		(half-frame (/ (frame-width) 2)))
	    (sql-beginning-of-row)
	    (scroll-left (- position half-frame))
	    (if (not horizontal-only)
		(progn
		  (sql-remove-header)
		  (recenter (1- (/ (window-height) 2)))
		  (sql-insert-header (window-start))))))
	(goto-char the-point))
    (recenter)))

(defun sql-toggle-results-in-new-frame ()
  "Toggle the value of sql-results-in-new-frame."
  (interactive)
  (setq sql-results-in-new-frame (not sql-results-in-new-frame)))

(defun sql-toggle-save-all-results ()
  "Toggle the value of sql-save-all-results."
  (interactive)
  (setq sql-save-all-results (not sql-save-all-results)))

(defun sql-pop-and-rename-buffer ()
  "Pop the current buffer to a new window, and give it a unique name."
  (interactive)
  (sql-goto-results-buffer)
  (or (eq major-mode 'sql-results-mode)
      (error "Buffer is not an sql-results buffer."))
  (let* ((buf-name (buffer-name))
	 (contents (buffer-substring (point-min) (point-max)))
	 (new-buffer (generate-new-buffer (concat buf-name "<saved>")))
	 (the-server sql-server)
	 (the-user sql-user))
    (switch-to-buffer-other-frame new-buffer)
    (insert contents)
    (sql-results-mode the-server the-user)
    (if sql-resize-results-frames
	(progn
	  (set-frame-height (selected-frame) sql-results-frame-height)
	  (set-frame-width (selected-frame) sql-results-frame-width)))))

(defun sql-drag-display (event)
  "Drag the current buffer under mouse."
  (interactive "@e")
  (let ((down t)
        (start-x (event-x event))
        (start-y (event-y event))
;        (p-shape x-pointer-shape)
;        (nt-p-shape x-nontext-pointer-shape)
;        (m-p-shape x-mode-pointer-shape)
        new-x
        new-y)
    (while down
      (next-event event)
      (if (or (button-press-event-p event)
              (button-release-event-p event))
          (setq down nil))
      (dispatch-event event)
      (if (motion-event-p event)
	(progn
	  (setq new-x (event-x event)
		new-y (event-y event))
	  (condition-case nil
	      (progn
		(sql-scroll-down (- new-y start-y))
		(scroll-left (- start-x new-x))
		(setq start-x new-x
		      start-y new-y))
	    (error "")))))))

(defun sql-set-sql-command (new-command)
  "Set the value of the variable sql-command."
  (interactive "sEnter the new sql-command: ")
  (if (not (eq 'sql-batch-mode major-mode))
      (error "You must be in a sql-batch-mode buffer to set `sql-command'.")
    (if new-command
	(progn
	  (setq sql-command new-command)
	  (sql-batch-mode sql-server sql-user sql-password nil t)))))

(defun sql-display-startup-message ()
  (interactive)
  (if (sit-for 3)
      (let ((lines sql-startup-message-lines))
	(message "SQL Mode Version %s, Copyright © 1994, 1995, 1996 Peter D. Pezaris"
		 sql-mode-version)
	(while (and (sit-for 4) lines)
	  (message (substitute-command-keys (car lines)))
	  (setq lines (cdr lines)))))
  (message ""))

(defun sql-interactive-prompt ()
  "Return a string to use as the prompt."
  (concat sql-command " [" sql-command-level "] -->"))

(defun sql-beginning-of-command-line ()
  "Goto the beginning of the current interactive line."
  (interactive)
  (beginning-of-line)
  (let* ((begin-point (point))
	 (end-point (progn (end-of-line) (point)))
	 (limit (if (> (- end-point begin-point) 6)
		    (min (progn (beginning-of-line) (forward-char 6) (point))
			 end-point)
		  end-point)))
    (beginning-of-line)
    (search-forward "> " limit t))
  (scroll-right sql-max-frame-width))

(defun sql-newline-maybe-indent ()
  "Insert a newline, and if sql-indent-after-newline is non-nil, indent."
  (interactive)
  (if (not sql-indent-after-newline)
      (newline)
    (sql-indent-line)
    (newline)
    (sql-indent-line)))

(defun sql-toggle-auto-indent ()
  "Toggle the state of the variable sql-indent-after-newline."
  (interactive)
  (if sql-indent-after-newline
      (setq sql-indent-after-newline nil)
    (setq sql-indent-after-newline t)))

(defun sql-backward-delete-char ()
  "Delete characters backward, stopping at prompt."
  (interactive)
  (let ((the-point (point)))
    (save-excursion
      (beginning-of-line)
      (if (and (looking-at "[0-9][0-9]*> ")
	       (search-forward "> " nil t))
	  (progn
	    (if (equal the-point (point))
		(beep)
	      (goto-char the-point)
	      (backward-delete-char-untabify 1)))
	(goto-char the-point)
	(backward-delete-char-untabify 1)))))

(defun sql-in-string ()
  "Return t if in a string, nil otherwise."
  (and sql-lucid (eq (buffer-syntactic-context) 'string)))

(defun sql-electric-delete (arg)
  "Deletes preceding character or whitespace.
If `sql-hungry-delete-key' is non-nil, then all preceding whitespace is
consumed.  If however an ARG is supplied, or `sql-hungry-delete-key' is
nil, or point is inside a string then the function in the variable
`sql-delete-function' is called."
  (interactive "P")
  (if (or (not sql-hungry-delete-key)
	  arg
	  (sql-in-string))
      (funcall sql-delete-function (prefix-numeric-value arg))
    (let ((here (point)))
      (skip-chars-backward " \t\n")
      (if (/= (point) here)
	  (delete-region (point) here)
	(funcall sql-delete-function 1)))))

(defun sql-previous-keyword ()
  "Return the previous keyword as a string.
See the variable `sql-keywords' for a list of keywords.
The search is bounded to the current line."
  (interactive)
  (save-excursion
    (let ((begin (save-excursion (beginning-of-line) (point))))
      (if (re-search-backward sql-keyword-regexp begin t)
	  (current-word)
	nil))))

(defun sql-previous-word (&optional count)
  "Return the word before the word at point as a string.
The search is bounded to the current line.
Optional argument COUNT specifies how many words to go backwards."
  (interactive "p")
  (setq count (or count 1))
  (save-excursion
    (let  ((pw nil))
      (while (< 0 count)
	(setq pw (sql-previous-word-1))
	(setq count (1- count))
	(or pw (setq count 0)))
      (cond
       ((stringp pw)
	pw)
       ((null pw)
	"")
       (t
	(current-word))))))
  
(defun sql-previous-word-1 ()
  (cond
   ((char-equal (preceding-char) ?,)
    (forward-char -1)
    ",")
   ((char-equal (preceding-char) ?=)
    (forward-char -1)
    "=")
   (t
    (re-search-backward " \\|\n" nil t)
    (cond
     ((char-equal (preceding-char) ?,)
      (forward-char -1)
      ",")
     ((char-equal (preceding-char) ?=)
      (forward-char -1)
      "=")
     (t
      (if (forward-word -1)
	  t
	nil))))))

(defun sql-display-completion-context ()
  "Display the completion context at point."
  (interactive)
  (message (symbol-name (sql-get-completion-context))))
  
(defun sql-get-completion-context (&optional location)
  "Get the completion context at point, or at optional LOCATION."
  (save-excursion
    (and location
	 (goto-char location))
    (let ((previous (sql-previous-word)))
      (and previous
	   (cond
	    ((or (string-equal (downcase previous) "select"))
	     'column)
	    ((or (string-equal (downcase previous) "into")
		 (string-equal (downcase previous) "from")
		 (string-equal (downcase previous) "update")
		 (string-equal (downcase previous) "delete")
		 (string-equal (downcase previous) "sp_helpindex")
		 )
	     'table)
	    ((or (string-equal (downcase previous) "where")
		 (string-equal (downcase previous) "set"))
	     'column)
	    ((and (string-equal (downcase previous) "by")
		  (string-equal (downcase (sql-previous-word 2)) "order"))
	     'column)
	    ((and (string-equal (downcase previous) "by")
		  (string-equal (downcase (sql-previous-word 2)) "group"))
	     'column)
	    ((and (string-equal (downcase previous) "table")
		  (string-equal (downcase (sql-previous-word 2)) "truncate"))
	     'table)
	    ((or (string-equal (downcase previous) "sp_helptext")
		 (string-equal (downcase previous) "exec"))
	     'stored-procedure)
	    ((or (string-equal (downcase previous) "use")
		 (string-equal (downcase previous) "sp_helpdb"))
	     'database)
	    ((or (string-equal (downcase previous) "sp_helpuser")
		 (string-equal (downcase previous) "to"))
	     'user)
	    ((or (string-equal (downcase previous) "and")
		 (string-equal (downcase previous) "or"))
	     (save-excursion
	       (re-search-backward sql-keyword-regexp nil t 2)
	       (forward-word 1)
	       (forward-char 1)
	       (sql-get-completion-context)))
	    ((string-equal (downcase previous) ",")
	     (save-excursion
	       (re-search-backward sql-keyword-regexp nil t 1)
	       (forward-word 2)
	       (sql-get-completion-context)))
	    ((member (downcase previous) sql-operators)
;	     (string-equal (downcase previous) "=")
	     'value)
	    ((null previous)
	     'keyword)
	    ((string-equal (downcase (sql-previous-word 2)) "where")
	     'operator)
	    (t
	     (if sql-lucid
		 (or (buffer-syntactic-context) 'keyword)
	       'keyword)))))))

(defun sql-get-column-alias ()
  "Get the alias of the current column, if any."
  (interactive)
  (let* ((space-point (save-excursion (search-backward " " nil t) (point)))
	 (dot (save-excursion (search-backward "." nil t))))
    (if (and dot (> dot space-point))
	(buffer-substring (1+ space-point) dot)
      nil)))

(defun sql-complete-word-maybe (arg)
  "Complete the word at point, or insert a tab.
If there is only whitespace between the beginning of the line and
point, insert a tab character (or whatever key this function is mapped
to), otherwise complete the word."
  (interactive "P")
  (let ((complete-it nil)
	(first-point (point)))
    (save-excursion
      (back-to-indentation)
      (if (< (point) first-point)
	  (setq complete-it t)))
    (if complete-it
	(let ((context (sql-get-completion-context)))
	  (cond
	   ((eq context 'keyword)
	    (if (not sql-keyword-list)
		(sql-get-keywords))
	    (sql-complete sql-keyword-list))
	   ((eq context 'table)
	    (if (not sql-table-list)
		(sql-get-tables))
	    (sql-complete sql-table-list))
	   ((eq context 'column)
	    (let* ((alias (sql-get-column-alias))
		   (table-name (sql-get-table-name alias)))
	      (if (not (assoc table-name sql-column-list))
		  (sql-get-columns table-name))
	      (sql-complete (cdr (assoc table-name sql-column-list)))))
	   ((eq context 'stored-procedure)
	    (if (not sql-stored-procedure-list)
		(sql-get-stored-procedures))
	    (sql-complete sql-stored-procedure-list))
	   ((eq context 'database)
	    (if (not sql-database-list)
		(sql-get-databases))
	    (sql-complete sql-database-list))
	   ((eq context 'user)
	    (if (not sql-user-list)
		(sql-get-users))
	    (sql-complete sql-user-list))
	   ((eq context 'operator)
	    (if (not sql-operator-list)
		(sql-get-operators))
	    (sql-complete sql-operator-list))
	   ((eq context 'value)
	    (let ((table-name (sql-get-table-name))
		  (column-name (sql-get-column-name)))
	      (if (not (assoc (cons table-name column-name) sql-value-list))
		  (sql-get-values table-name column-name))
	      (sql-complete (cdr (assoc (cons table-name column-name)
					sql-value-list)))))
;	    (message "Unable to complete (value)."))
	   (t
	    (message "Unknown context."))))
      ; insert a tab
      (self-insert-command (prefix-numeric-value arg)))))

(defun sql-complete (completion-table)
  "Complete the word at point based on COMPLETION-TABLE."
  (let* ((end (point))
	 (begin (save-excursion (re-search-backward " \\|\\.\\|\n" nil t)
				(+ 1 (point))))
;	 (partial (current-word))
	 (partial (if (or (eq (preceding-char) ?,)
			  (eq (preceding-char) ?=))
		      ""
		    (buffer-substring begin end)))
	 (complete (try-completion partial completion-table))
	 (all-complete (all-completions partial completion-table)))
    (cond
     ((eq complete t)
      (insert " ")
      (message "Sole completion."))
     ((eq complete nil)
      (message "No match.")
      (and sql-noisy
	   sql-xemacs
	   (assoc 'no-completion sound-alist)
	   (play-sound 'no-completion)))
     (t
      (if (string-equal complete partial)
	  (sql-dynamic-list-completions all-complete)
;	(delete-region begin end)
	(cond
	 ((eq (preceding-char) ?\")
	  (delete-char -1))
	 ((not (eq (preceding-char) ? ))
	  (backward-kill-word 1)
	  (if (eq (preceding-char) ?\")
	      (delete-char -1)))
	 (t
	  nil))
	(insert complete)
	(if (equal 1 (length all-complete))
	    (progn
	      (insert " ")
	      (message "Sole completion."))
	  (if (assoc complete completion-table)
	      (message "Complete, but not unique.")
	    (message "Partial Completion."))))))))

(defun sql-dynamic-list-completions (completions)
  "List in help buffer sorted COMPLETIONS.
Typing anything but `s' flushes the help buffer."
  (let ((conf (current-window-configuration)))
    (with-output-to-temp-buffer " *Completions*"
      (display-completion-list (sort completions 'string-lessp)))
    (and sql-xemacs
	 (not sql-xemacs-19-12)
	 (progn
	   (set-buffer " *Completions*")
	   (goto-char (point-min))
	   (set-syntax-table sql-mode-syntax-table)
	   (forward-line 1)
	   (skip-chars-forward "[ \t\n]")
	   (let ((start (point)))
	     (while (forward-word 1)
	       (let ((e (make-extent start (point))))
		 (set-extent-property e 'highlight t)
		 (skip-chars-forward "[ \t\n]")
		 (setq start (point)))))))
    (sql-restore-window-config conf)))

(defun sql-restore-window-config (conf &optional message)
  (message "%s" (or message
		    "Press `s' to save completions, anything else to flush."))
  (sit-for 0)
  (setq sql-temp-string nil)
  (if (if (fboundp 'next-command-event)
          ;; lemacs
          (let ((ch (next-command-event)))
            (if (eq (event-to-character ch) ?s)
                t
	      (progn (if (and (button-event-p ch) (eq (event-button ch) 2))
			 (setq sql-temp-string (save-excursion
						 (mouse-set-point ch)
						 (current-word)))
		       (setq unread-command-event ch))
		     nil)))
	;; v19 FSFmacs
	(let ((ch (read-event)))
	  (if (eq ch ?s)
	      t
	    (progn (setq unread-command-events (list ch))
		   nil))))
      nil
;      (message nil)
    (set-window-configuration conf)
    (and sql-temp-string
	 (progn
	   (or (eq (preceding-char) ? )
	       (eq (preceding-char) ?,)
	       (eq (preceding-char) ?.)
	       (eq (preceding-char) ?=)
	       (backward-kill-word 1))
	   (insert sql-temp-string " "))))
  (message nil))

(defun sql-split-window-horizontally ()
  "Split the window horizontally at the current column."
  (interactive)
  (let* ((window (split-window-horizontally (+ 3 (- (current-column)
						    (window-hscroll))))))
    (setq sql-linked-windows (cons window sql-linked-windows))
    (other-window 1)
    (set-window-hscroll (selected-window) (current-column))))

(defun sql-unsplit-window-horizontally ()
  "Unsplit the window horizontally."
  (interactive)
  (let ((window (selected-window)))
    (if (member window sql-linked-windows)
	(setq sql-linked-windows (delete window sql-linked-windows))))
  (delete-window))

(defun sql-grow-window-horizontally ()
  (interactive)
  (shrink-window-horizontally -1))

(defun sql-get-tables ()
  "Set the variable `sql-table-list' to the tables in the current database."
  (interactive)
  (setq sql-modified-cache t)
  (message "Creating table completion list... (querying database...)")
  (let* ((the-old-window (selected-window))
	 (the-old-buffer (current-buffer))
	 (the-old-point (point))
	 (temp-buffer (get-buffer-create " SQL-TEMP"))
	 (command (concat (if sql-database (concat "use " sql-database "\ngo\n") "")
			  sql-get-tables-command))
	 (sql-getting-completions t)
	 (sql-completion-buffer temp-buffer)
	 (sql-evaluation-method 'foreground))
    (setq sql-old-point (point))
    (set-buffer temp-buffer)
    (erase-buffer)
    (set-buffer the-old-buffer)
    (sql-evaluate command sql-server sql-user sql-password sql-batch-command-switches
		  sql-matching-buffer t)
    (message "Creating table completion list... (parsing results...)")
    (set-buffer temp-buffer)
    (goto-char (point-min))
    (and (looking-at sql-error-regexp)
	 (progn
	   (kill-buffer temp-buffer)
	   (error "Error parsing tables.")))
    (goto-char (point-max))
    (forward-line -2)
    (kill-line 3)
    (goto-char (point-min))
    (kill-line 2)
    (let ((new-table-list nil))
      (while (re-search-forward sql-word-regexp nil t)
	(setq new-table-list
	      (cons (cons (match-string 0) "1") new-table-list)))
      (kill-buffer temp-buffer)
      (set-buffer the-old-buffer)
      (setq sql-table-list new-table-list))
    (select-window the-old-window)
    (goto-char the-old-point)
    (message "Creating table completion list... done")))
  
(defun sql-complete-table-list (string ignore action)
  (let ((old-buffer (current-buffer)))
    (set-buffer sql-completion-saved-buffer)
    (or sql-table-list (sql-get-tables))
    (let ((tables sql-table-list))
      (set-buffer old-buffer)
      (cond ((null action)
	     (try-completion string tables))
	    ((eq action 'lambda)
	     (member string (mapcar 'car tables)))
	    (t
	     (all-completions string tables))))))

(defun sql-complete-column-list (string ignore action)
  (let ((old-buffer (current-buffer)))
    (set-buffer sql-completion-saved-buffer)
    (or (assoc sql-completion-saved-table sql-column-list)
	(sql-get-columns sql-completion-saved-table))
    (let ((columns (reverse (cdr (assoc sql-completion-saved-table
					sql-column-list)))))
      (set-buffer old-buffer)
      (cond ((null action)
	     (try-completion string columns))
	    ((eq action 'lambda)
	     (member string (mapcar 'car columns)))
	    (t
	     (all-completions string columns))))))

(defun sql-complete-value-list (string ignore action)
  (let ((old-buffer (current-buffer)))
    (set-buffer sql-completion-saved-buffer)
    (or (assoc (cons sql-completion-saved-table sql-completion-saved-column)
	       sql-value-list)
	(sql-get-values sql-completion-saved-table sql-completion-saved-column))
    (let ((values (reverse (cdr (assoc (cons sql-completion-saved-table
					     sql-completion-saved-column)
				       sql-value-list)))))
      (set-buffer old-buffer)
      (cond ((null action)
	     (try-completion string values))
	    ((eq action 'lambda)
	     (member string (mapcar 'car values)))
	    (t
	     (all-completions string values))))))

(defun sql-complete-stored-procedure-list (string ignore action)
  (let ((old-buffer (current-buffer)))
    (set-buffer sql-completion-saved-buffer)
    (or sql-stored-procedure-list (sql-get-stored-procedures))
    (let ((stored-procedures sql-stored-procedure-list))
      (set-buffer old-buffer)
      (cond ((null action)
	     (try-completion string stored-procedures))
	    ((eq action 'lambda)
	     (member string (mapcar 'car stored-procedures)))
	    (t
	     (all-completions string stored-procedures))))))
  
(defun sql-complete-database-list (string ignore action)
  (let ((old-buffer (current-buffer)))
    (set-buffer sql-completion-saved-buffer)
    (or sql-database-list (sql-get-databases))
    (let ((databases sql-database-list))
      (set-buffer old-buffer)
      (cond ((null action)
	     (try-completion string databases))
	    ((eq action 'lambda)
	     (member string (mapcar 'car databases)))
	    (t
	     (all-completions string databases))))))

(defun sql-complete-user-list (string ignore action)
  (let ((old-buffer (current-buffer)))
    (set-buffer sql-completion-saved-buffer)
    (or sql-user-list (sql-get-users))
    (let ((users sql-user-list))
      (set-buffer old-buffer)
      (cond ((null action)
	     (try-completion string users))
	    ((eq action 'lambda)
	     (member string (mapcar 'car users)))
	    (t
	     (all-completions string users))))))

(defun sql-get-databases ()
  "Set the variable `sql-database-list' to the databases on the current server."
  (interactive)
  (setq sql-modified-cache t)
  (message "Creating database completion list... (querying database...)")
  (let* ((the-old-window (selected-window))
	 (the-old-buffer (current-buffer))
	 (the-old-point (point))
	 (temp-buffer (get-buffer-create " SQL-TEMP"))
	 (command sql-get-databases-command)
	 (sql-getting-completions t)
	 (sql-completion-buffer temp-buffer)
	 (sql-evaluation-method 'foreground))
    (setq sql-old-point (point))
    (set-buffer temp-buffer)
    (erase-buffer)
    (set-buffer the-old-buffer)
    (sql-evaluate command sql-server sql-user sql-password sql-batch-command-switches
		  sql-matching-buffer t)
    (message "Creating database completion list... (parsing results...)")
    (set-buffer temp-buffer)
    (and (looking-at sql-error-regexp)
	 (progn
	   (kill-buffer temp-buffer)
	   (error "Error parsing tables.")))
    (goto-char (point-max))
    (forward-line -2)
    (kill-line 3)
    (goto-char (point-min))
    (kill-line 2)
    (let ((new-database-list nil))
      (while (re-search-forward sql-word-regexp nil t)
	(setq new-database-list
	      (cons (cons (match-string 0) "1") new-database-list))
	(forward-line 1))
      (kill-buffer temp-buffer)
      (set-buffer the-old-buffer)
      (setq sql-database-list new-database-list))
    (select-window the-old-window)
    (goto-char the-old-point)
    (message "Creating database completion list... done")))

(defun sql-get-stored-procedures ()
  "Set the variable `sql-stored-procedure-list' to the stored procedures
in the current database."
  (interactive)
  (setq sql-modified-cache t)
  (message "Creating stored procedure completion list... (querying database...)")
  (let* ((the-old-window (selected-window))
	 (the-old-buffer (current-buffer))
	 (the-old-point (point))
	 (temp-buffer (get-buffer-create " SQL-TEMP"))
	 (command (concat (if sql-database (concat "use " sql-database "\ngo\n") "")
			  sql-get-stored-procedures-command))
	 (sql-getting-completions t)
	 (sql-completion-buffer temp-buffer)
	 (sql-evaluation-method 'foreground))
    (setq sql-old-point (point))
    (set-buffer temp-buffer)
    (erase-buffer)
    (set-buffer the-old-buffer)
    (sql-evaluate command sql-server sql-user sql-password sql-batch-command-switches
		  sql-matching-buffer t)
    (message "Creating stored procedure completion list... (parsing results...)")
    (set-buffer temp-buffer)
    (goto-char (point-min))
    (and (looking-at sql-error-regexp)
	 (progn
	   (kill-buffer buffer)
	   (error "Error parsing stored procedures.")))
    (goto-char (point-max))
    (forward-line -2)
    (kill-line 3)
    (goto-char (point-min))
    (kill-line 2)
    (let ((new-stored-procedure-list nil))
      (while (re-search-forward sql-word-regexp nil t)
	(setq new-stored-procedure-list
	      (cons (cons (match-string 0) "1") new-stored-procedure-list)))
      (kill-buffer temp-buffer)
      (set-buffer the-old-buffer)
      (setq sql-stored-procedure-list new-stored-procedure-list))
    (select-window the-old-window)
    (goto-char the-old-point)
    (message "Creating stored procedure completion list... done")))

(defun sql-get-users ()
  "Set the variable `sql-user-list' to the users
in the current database."
  (interactive)
  (setq sql-modified-cache t)
  (message "Creating user completion list... (querying database...)")
  (let* ((the-old-window (selected-window))
	 (the-old-buffer (current-buffer))
	 (the-old-point (point))
	 (temp-buffer (get-buffer-create " SQL-TEMP"))
	 (command (concat (if sql-database (concat "use " sql-database "\ngo\n") "")
			  sql-get-users-command))
	 (sql-getting-completions t)
	 (sql-completion-buffer temp-buffer)
	 (sql-evaluation-method 'foreground))
    (setq sql-old-point (point))
    (set-buffer temp-buffer)
    (erase-buffer)
    (set-buffer the-old-buffer)
    (sql-evaluate command sql-server sql-user sql-password sql-batch-command-switches
		  sql-matching-buffer t)
    (message "Creating user completion list... (parsing results...)")
    (set-buffer temp-buffer)
    (goto-char (point-min))
    (and (looking-at sql-error-regexp)
	 (progn
	   (kill-buffer buffer)
	   (error "Error parsing users.")))
    (goto-char (point-max))
    (forward-line -2)
    (kill-line 3)
    (goto-char (point-min))
    (kill-line 2)
    (let ((new-user-list nil))
      (while (re-search-forward sql-word-regexp nil t)
	(setq new-user-list
	      (cons (cons (match-string 0) "1") new-user-list)))
      (kill-buffer temp-buffer)
      (set-buffer the-old-buffer)
      (setq sql-user-list new-user-list))
    (select-window the-old-window)
    (goto-char the-old-point)
    (message "Creating user completion list... done")))

(defun sql-get-table-name (&optional alias)
  "Get the name of the table to do the completion.

If optional ALIAS is non-nil look for the table with that alias."
  (if alias
      (save-excursion
	(re-search-backward sql-table-prefix-regexp nil t)
	(re-search-forward (concat " " alias "\\>"))
	(forward-word -2)
	(current-word))
    (save-excursion
      (end-of-line)
      (re-search-backward sql-table-prefix-regexp nil t)
      (forward-word 2)
      (let ((table (current-word)))
	(if (string-equal table "from")
	    (or sql-last-table "")
	  (if (and sql-last-table
		   (or (string-equal table "")
		       (and sql-table-list
			    (not (assoc table sql-table-list)))))
	      sql-last-table
	    table))))))

(defun sql-get-column-name ()
  "Get the name of the column to do the completion."
  (save-excursion
    (re-search-backward sql-operator-regexp nil t)
    (sql-previous-word)))
;  (if (eq (preceding-char) ? )
;      (current-word)
;    (sql-previous-word 2)))

(defun sql-get-columns (&optional table)
  "Set the variable `sql-column-list' to the columns in the current database.

If optional TABLE is non-nill, use that string as the table to get the columns
from.  Otherwise, scan backwards looking for something that looks like a table
name."
  (interactive)
  (setq sql-modified-cache t)
  (message "Creating column completion list... (querying database...)")
  (let* ((the-old-window (selected-window))
	 (the-old-buffer (current-buffer))
	 (the-old-point (point))
	 (temp-buffer (get-buffer-create " SQL-TEMP"))
	 (table-name (or table (sql-get-table-name)))
	 (command (concat (if sql-database (concat "use " sql-database "\ngo\n") "")
			  sql-get-columns-command-prefix
			  table-name
			  sql-get-columns-command-suffix))
	 (sql-getting-completions t)
	 (sql-completion-buffer temp-buffer)
	 (sql-evaluation-method 'foreground))
    (setq sql-old-point (point))
    (set-buffer temp-buffer)
    (erase-buffer)
    (set-buffer the-old-buffer)
    (sql-evaluate command sql-server sql-user sql-password sql-batch-command-switches
		  sql-matching-buffer t)
    (message "Creating column completion list... (parsing results...)")
    (set-buffer temp-buffer)
    (goto-char (point-min))
    (and (looking-at sql-error-regexp)
	 (progn
	   (kill-buffer buffer)
	   (error "Error parsing columns.")))
    (goto-char (point-max))
    (forward-line -2)
    (kill-line 3)
    (goto-char (point-min))
    (kill-line 2)
    (let ((new-column-list nil)
	  (column nil)
	  (type nil))
      (while (re-search-forward sql-word-regexp nil t)
	(setq column (match-string 0))
	(re-search-forward sql-word-regexp nil t)
	(setq type (match-string 0))
	(setq new-column-list
	      (cons (cons column (cond
				  ((member type sql-string-column-types)
				   1)
				  ((member type sql-ignore-column-types)
				   2)
				  (t
				   0)))
		    new-column-list)))
	(kill-buffer temp-buffer)
	(set-buffer the-old-buffer)
	(setq sql-column-list (cons (cons table-name new-column-list)
				    sql-column-list)))
    (select-window the-old-window)
    (goto-char the-old-point)
    (message "Creating column completion list... done")))

(defun sql-get-values (&optional table column)
  "Set the variable `sql-value-list' to the values in the current database.

If optional TABLE is non-nill, use that string as the table to get the values
from.  Otherwise, scan backwards looking for something that looks like a table
name."
  (interactive)
  (setq sql-modified-cache t)
  (message "Creating value completion list... (querying database...)")
  (let* ((the-old-window (selected-window))
	 (the-old-buffer (current-buffer))
	 (the-old-point (point))
	 (temp-buffer (get-buffer-create " SQL-TEMP"))
	 (table-name (or table (sql-get-table-name)))
	 (column-name (or column (sql-get-column-name)))
	 (command (concat (if sql-database (concat "use " sql-database "\ngo\n") "")
			  sql-get-values-command-prefix
			  column-name
			  sql-get-values-command-middle
			  table-name
			  sql-get-values-command-suffix))
	 (sql-getting-completions t)
	 (sql-completion-buffer temp-buffer)
	 (sql-evaluation-method 'foreground)
	 (column-list (cdr (assoc table-name sql-column-list))))
    (setq sql-old-point (point))
    (set-buffer temp-buffer)
    (erase-buffer)
    (set-buffer the-old-buffer)
    (sql-evaluate command sql-server sql-user sql-password
		  sql-batch-command-switches sql-matching-buffer t)
    (message "Creating value completion list... (parsing results...)")
    (set-buffer temp-buffer)
;    (error "FOO")
    (goto-char (point-min))
    (and (looking-at sql-error-regexp)
	 (progn
	   (kill-buffer buffer)
	   (error "Error parsing values.")))
    (goto-char (point-max))
    (forward-line -2)
    (kill-line 3)
    (goto-char (point-min))
    (kill-line 2)
    (let* ((new-value-list nil)
	   (column-type (eq 0 (cdr (assoc column-name column-list)))))
      (while (re-search-forward sql-line-regexp nil t)
	(let* ((value (match-string 1))
	       (no-quotes (or column-type
			      (string-equal "NULL" value)
			      (string-match "\"\\|'" value)
			      (string-equal "null" value)
			      (string-match "\(.*\)" value))))
	  (setq value (if no-quotes
			  value
			(concat "\"" value "\"")))
	  (setq new-value-list
		(cons (cons value "1") new-value-list))))
      (kill-buffer temp-buffer)
      (set-buffer the-old-buffer)
      (setq sql-value-list (cons (cons (cons table-name
					     column-name)
				       new-value-list)
				 sql-value-list)))
    (select-window the-old-window)
    (goto-char the-old-point)
    (message "Creating value completion list... done")))

(defun sql-clear-cached-data ()
  "Clear the cached table, column, and stored procedure data.
This function sets the value of the variables `sql-table-list',
`sql-column-list', `sql-value-list', `sql-stored-procedure-list'
and `sql-user-list' to nil."
  (interactive)
  (setq sql-table-list nil)
  (setq sql-column-list nil)
  (setq sql-value-list nil)
  (setq sql-stored-procedure-list nil))

(defun sql-clear-cached-table-data ()
  "Clear the cached table data."
  (interactive)
  (setq sql-table-list nil))

(defun sql-clear-cached-column-data ()
  "Clear the caced column data."
  (interactive)
  (setq sql-column-list nil))

(defun sql-clear-cached-value-data ()
  "Clear the caced value data."
  (interactive)
  (setq sql-value-list nil))

(defun sql-clear-cached-stored-procedure-data ()
  "Clear the cached stored procedure data."
  (interactive)
  (setq sql-stored-procedure-list nil))

(defun sql-clear-cached-database-data ()
  "Clear the cached database data."
  (interactive)
  (setq sql-database-list nil))

(defun sql-clear-cached-user-data ()
  "Clear the cached user data."
  (interactive)
  (setq sql-user-list nil))

(defun sql-get-keywords ()
  "Set the variable `sql-keyword-list' to current sql keywords."
  (let ((keys sql-keywords))
    (while keys
      (setq sql-keyword-list (cons (cons (car keys) "1") sql-keyword-list))
      (setq keys (cdr keys)))))

(defun sql-get-operators ()
  "Set the variable `sql-operator-list' to current sql operators."
  (let ((opers sql-operators))
    (while opers
      (setq sql-operator-list (cons (cons (car opers) "1") sql-operator-list))
      (setq opers (cdr opers)))))

(defun sql-set-database (new-database)
  "Set the database to use in the current buffer."
  (interactive (list
		(progn
		  (setq sql-completion-saved-buffer (current-buffer))
		  (completing-read "Database: " 'sql-complete-database-list))))
  (if (string-equal new-database "")
      (sql-reset-database)
;    (and sql-save-cache-on-exit (sql-save-cache-data))
    (sql-clear-cached-data)
    (setq sql-database new-database)
    (setq sql-database-name (concat "   " new-database))
    (and sql-always-load-cache (sql-load-cache-data))
    (and sql-always-load-history (sql-load-history))))

(defun sql-reset-database ()
  "Set the database to use in the current buffer to nil."
  (interactive)
  (sql-clear-cached-data)
  (setq sql-database nil)
  (setq sql-database-name nil))

(defun sql-abort ()
  "Abort the current query."
  (interactive)
  (and sql-process (kill-process sql-process))
  (setq modeline-process nil)
  (call-interactively 'keyboard-quit))

(defun sql-goto-batch-buffer ()
  (interactive)
  (if (not (eq major-mode 'sql-batch-mode))
      (let ((m-buffer sql-matching-buffer))
	(or m-buffer (error "No matching batch buffer."))
	(set-buffer m-buffer)
	(if sql-results-in-new-frame
	    (select-window (car (windows-of-buffer m-buffer)))
	  (if sql-lucid
	      (pop-to-buffer m-buffer nil sql-frame)
	    (pop-to-buffer m-buffer nil)))
	(if (not (eq major-mode 'sql-batch-mode))
	    (error "Can't find matching batch buffer.")))))

(defun sql-goto-results-buffer ()
  (interactive)
  (if (not (eq major-mode 'sql-results-mode))
      (let ((m-buffer sql-matching-buffer))
	(or m-buffer (error "No matching results buffer."))
	(set-buffer m-buffer)
	(if sql-results-in-new-frame
	    (select-window (car (windows-of-buffer m-buffer)))
	  (if sql-lucid
	      (pop-to-buffer m-buffer nil sql-frame)
	    (pop-to-buffer m-buffer nil)))
	(if (not (eq major-mode 'sql-results-mode))
	    (error "Can't find matching results buffer.")))))

(defun sql-print-buffer ()
  "Print the current buffer, using `sql-print-buffer-tiled' if appropriate."
  (interactive)
  (cond
   ((eq major-mode 'sql-interactive-mode)
    (sql-print-buffer-tiled))
   ((eq major-mode 'sql-results-mode)
    (sql-print-buffer-tiled))
   (t
    (let ((lpr-switches (or sql-print-switches lpr-switches))
	  (enscript-switches (or sql-print-switches enscript-switches)))
      (if (eq sql-print-command 'enscript-buffer)
	  (enscript-buffer nil)
	(funcall sql-print-command))))))

(defun sql-insert-sp (&optional bypass-cpp)
  "Insert a stored procedure from a file into the current buffer.
The file is run throught the C preprocessor, and then ` ;' strings are
replaced with newlines.

With prefix arg bypass the C preprocessor step.  If the variable
`sql-bypass-cpp' is non-nil, bypass the C preprocessor step.

The file is then inserted into the current sql-batch-mode buffer."
  (interactive "P")
  (let ((sql-holdup-stored-procedure t)
	(sql-bypass-cpp bypass-cpp))
    (call-interactively 'sql-load-sp)))

(defun sql-load-sp (filename &optional bypass-cpp)
  "Load a stored procedure from a file.
The file is run throught the C preprocessor, and then ` ;' strings are
replaced with newlines.

With prefix arg bypass the C preprocessor step.  If the variable
`sql-bypass-cpp' is non-nil, bypass the C preprocessor step.

The file is then loaded into the current sql-batch-mode buffer, and
sql-evaluate-buffer is called to load the stored procedure.

If sql-holdup-stored-procedure is non-nil, the buffer is not evaluated."
  (interactive "fStored Procedure file name: 
P")
  (if (not (file-readable-p filename))
      (error "Could not read file %s." filename)
    (sql-goto-batch-buffer)
    (setq filename (expand-file-name filename))
    (if (or bypass-cpp sql-bypass-cpp)
	(insert-file filename)
      (let* ((original-buffer (current-buffer))
	     (temp-buffer (get-buffer-create " SQL-TEMP"))
	     (prompt (concat "cpp "
			     (or sql-default-cpp-switches
				 "-B -C -DSQL -P ")))
	     (cpp-switches (read-shell-command
			    "Run the C preprocessor like this: "
			    prompt))
	     (default-directory (file-name-directory filename)))
	(call-process shell-file-name nil temp-buffer nil
		      "-c" (concat cpp-switches filename))
	(set-buffer temp-buffer)
;	(sql-insert-newlines)
	(let ((contents (buffer-string)))
	  (set-buffer original-buffer)
	  (kill-buffer temp-buffer)
	  (sql-goto-history 0 nil)
	  (insert contents))))
    (goto-char (point-min))
    (or sql-holdup-stored-procedure
	(let ((sql-confirm-changes nil))
	  (sql-evaluate-buffer nil)))))

(defun sql-new-query ()
  "Clear the batch buffer and results buffer contents."
  (interactive)
  (sql-goto-batch-buffer)
  (sql-goto-history 0 nil))

(defun sql-insert-file ()
  "Insert the contents of a file into the batch buffer."
  (interactive)
  (sql-goto-batch-buffer)
  (sql-goto-history 0 nil)
  (call-interactively 'insert-file))

;(defun sql-in-results-buffer (body)
;  "Switch to results buffer, eval body, then switch back."
;  (let ((this-buffer (current-buffer))
;	(matching-buffer sql-matching-buffer))
;    (if (eq major-mode 'sql-batch-mode)
;	(progn
;	  (set-buffer matching-buffer)
;	  (eval body)
;	  (set-buffer this-buffer))
;      (eval body))))

;(defun sql-dynamic-scroll-up (event)
;  "Scroll up by a line, a screen, or to top.
;Click and hold to scroll a line at a time.
;Single click to scroll by a screen.
;Double click to scroll to top."
;  (interactive "e")
;  (let* ((this-time (event-timestamp event))
;	 (difference (- this-time sql-last-scroll-time)))
;    (message "Difference %d" difference)
;    (sit-for 3)
;    (sql-in-results-buffer
;     (if (> 300 difference)
;	 (sql-end-of-buffer)
;       (sql-scroll-up)))
;    (setq sql-last-scroll-time (event-timestamp event))))

;(defun sql-dynamic-scroll-down (event)
;  "Scroll down by a line, a screen, or to top.
;Click and hold to scroll a line at a time.
;Single click to scroll by a screen.
;Double click to scroll to top."
;  (interactive "e")
;  (let* ((this-time (event-timestamp event))
;	 (difference (- this-time sql-last-scroll-time)))
;    (sql-in-results-buffer
;     (if (> 300 difference)
;	 (sql-beginning-of-buffer)
;       (sql-scroll-down)))
;    (setq sql-last-scroll-time (event-timestamp event))))

(defun sql-toggle-font-lock (mode)
  "Toggle the state of font-locking for MODE."
  (interactive)
  (if (eq sql-font-lock-buffers 'all)
      (setq sql-font-lock-buffers (list 'sql-mode 
					'sql-batch-mode
					'sql-interactive-mode
					'sql-results-mode)))
  (if (member mode sql-font-lock-buffers)
      (setq sql-font-lock-buffers (delete mode sql-font-lock-buffers))
    (setq sql-font-lock-buffers (cons mode sql-font-lock-buffers))))

(defun sql-beginning-of-buffer ()
  "Move point to beginning of buffer, keeping header text at top of window."
  (interactive)
  (push-mark)
  (sql-remove-header)
  (goto-char (point-min))
  (sql-beginning-of-row)
  (sql-insert-header))

(defun sql-end-of-buffer ()
  "Move point to end of buffer, keeping header text at top of window."
  (interactive)
  (push-mark)
  (goto-char (point-max))
  (sql-beginning-of-row)
  (sql-recenter))

(defun sql-scroll-down-one-line ()
  "Scroll the text of the current window downward one line."
  (interactive)
  (let ((previous scroll-previous-lines))
    (sql-scroll-down 1)
    (setq scroll-previous-lines previous)))
   
(defun sql-scroll-up-one-line ()
  "Scroll the text of the current window upward one line."
  (interactive)
  (let ((previous scroll-previous-lines))
    (sql-scroll-up 1)
    (setq scroll-previous-lines previous)))
   
(defun sql-scroll-up (&optional lines)
  "Scroll up."
  (interactive)
  (if (equal (point-max) (window-end))
      (error "End of buffer")
    (sql-remove-header)
    (scroll-up-in-place lines)
    (sql-insert-header (window-start))))

(defun sql-scroll-down (&optional lines)
  (interactive)
  (if (equal 1 (window-start))
      (message "Beginning of buffer")
    (sql-remove-header)
    (scroll-down-in-place lines)
    (sql-insert-header (window-start))))

(defun sql-move-header-toggle ()
  "Toggle the moving of headers withing the results buffers."
  (interactive)
  (if sql-move-headers
      (message "Move headers: inactive.")
    (message "Move headers: active."))
  (setq sql-move-headers (not sql-move-headers)))

(defun sql-remove-header ()
  "Remove the header of the results buffer from the top of the window."
  (interactive)
  (and sql-global-move-headers
       sql-move-headers
       sql-header-text
       (save-excursion
	 (let ((marking-changes sql-marking-changes))
	   (sql-stop-marking-changes)
	   (goto-char (point-min))
	   (while (search-forward sql-header-text nil t)
	     (replace-match "" nil t))
	   (if marking-changes
	       (sql-start-marking-changes))
	   (match-beginning 0)))))
  
(defun sql-insert-header (&optional location)
  "Insert the header of the results buffer at point, or at optional LOCATION."
  (interactive)
  (and sql-global-move-headers
       sql-move-headers
       sql-header-text
       (save-excursion
	 (let ((current-point (or location (point)))
	       (marking-changes sql-marking-changes))
	   (sql-stop-marking-changes)
	   (goto-char current-point)
	   (beginning-of-line)
	   (insert sql-header-text)
	   (if marking-changes
	       (sql-start-marking-changes))))))

(defun sql-insert-gos ()
  "Inserts `go' statements between each apparent block of SQL code.
Good for making a SQL script program out of plain SQL."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (forward-line 1)
      (if (and (looking-at "[a-z]") (not (looking-at "go")))
	  (insert "go\n")))
    (insert "go\n")))

(defun sql-insert-semi-colons ()
  "Inserts `;'s between each apparent block of SQL code.
Good for making a SQL script program out of plain SQL."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (forward-line 1)
      (if (and (looking-at "[a-z]") (save-excursion
				      (forward-char -2)
				      (not (looking-at ";"))))
	  (progn
	    (forward-char -1)	    
	    (insert ";")
	    (forward-char 1))))
    (forward-char -1)
    (insert ";")))

(defun sql-set-interfaces-file-name ()
  "Determine where the interfaces file is, if possible."
  (let ((sybase (getenv "SYBASE"))
	(sybase-home (expand-file-name "~sybase"))
	(interfaces "/interfaces"))
    (cond ((file-readable-p (concat sybase interfaces))
	   (setq sql-interfaces-file-name (concat sybase interfaces)))
	  ((file-readable-p (concat sybase-home interfaces))
	   (setq sql-interfaces-file-name (concat sybase-home interfaces)))
	  (t (setq sql-interfaces-file-name nil))))
  sql-interfaces-file-name)

(defun sql-read-interfaces-file (&optional error-message)
  "Read and parse the interfaces file specifiled by `sql-interfaces-file-name'."
  (if (and (not sql-interfaces-file-name)
	   (not (sql-set-interfaces-file-name)))
      (if error-message
	  (error "Could not find the interfaces file.")
	(message "Could not find the interfaces file.")
	(sit-for 2))
    (message "Reading interfaces file %s..." sql-interfaces-file-name)
    (let ((interfaces-buffer (find-file-noselect sql-interfaces-file-name t)))
      (set-buffer interfaces-buffer)
      ; process the lines one at a time
      (while (not (eobp))
	(back-to-indentation)
	(cond ((looking-at "query ")
	       nil) ;(add-to-hosts))
	      ((looking-at "master ")
	       nil)
	      ((looking-at "console ")
	       nil)
	      ((looking-at "debug ")
	       nil)
	      ((looking-at "trace ")
	       nil)
	      ((looking-at "#")
	       nil)
	      ((looking-at "\n")
	       nil)
	      ((equal (current-column) 0)
	       (if (not (member (cons (current-word) 1) sql-server-table))
		   (setq sql-server-table
			 (cons (cons (current-word) 1) sql-server-table))))
	      (t
	       (message "Unable to parse line %d in interfaces file"
			(count-lines (point-min) (point)))
	       (sit-for 1)))
	(forward-line 1))
      (kill-buffer interfaces-buffer))
    (message "Reading interfaces file %s... done" sql-interfaces-file-name)))

(defun skip-whitespace ()
  "Search forward for the first character that isn't a SPACE, TAB or NEWLINE."
  (interactive)
  (while (looking-at "[ \t\n]")
    (forward-char 1)))

(defun sql-set-sybase ()
  "Set the value of the SYBASE environment variable.
The value is read from the minibuffer."
  (interactive)
  (let* ((old-sybase (getenv "SYBASE"))
	 (prompt "Value for SYBASE environment variable")
	 (prompt2 (if old-sybase
		      (format " (old value = '%s'): " old-sybase)
		    ": ")))
    (setenv "SYBASE" (read-from-minibuffer (concat prompt prompt2)))
    (sql-read-interfaces-file)))

(defun sql-what-column ()
  (interactive)
  "Return the column number and column name at point."
  (or (eq major-mode 'sql-results-mode)
      (error "You must be in a sql-results-mode buffer."))
  (if (not sql-column-separator)
      (sql-guess-what-column)
    (save-excursion
      (let ((old-point (point))
	    (column-number -1)
	    (column-name nil)
	    (counter 0))
	(beginning-of-line)
	(while (< (point) old-point)
	  (search-forward sql-column-separator nil t)
	  (setq column-number (1+ column-number)))
	(if (and sql-global-move-headers
		 sql-move-headers
		 sql-header-text)
	    (goto-char (window-start))
	  (goto-char (point-min)))
	(search-forward sql-column-separator nil t (1- column-number))
	(setq column-name (current-word))
	(message "Column #%d %s" column-number column-name)))))

(defun sql-guess-what-column ()
  (interactive)
  (save-excursion
    (let ((column (current-column)))
      (if (and sql-global-move-headers
	       sql-move-headers
	       sql-header-text)
	  (goto-char (window-start))
	(goto-char (point-min)))
      (goto-column column)
      (message "Column %s" (current-word)))))
  
(defun sql-set-variable (variable &optional number-only)
  "Set the value of a sql variable."
  (interactive)
  (if (symbolp variable)
      (let* ((prompt (format "New value for %s (currently %s): "
			     (symbol-name variable)
			     (symbol-value variable)))
	     (response (read-from-minibuffer prompt nil))
	     (new-value (if number-only
			    (string-to-int response)
			  response)))
	(if response
	    (set variable new-value)))))

(defun sql-flatten-form (var)
  (princ "  ")
  (if (symbolp var)
      (prin1 (list 'setq var
		   (let ((val (symbol-value var)))
		     (if (or (memq val '(t nil))
			     (and (not (symbolp val))
				  (not (listp val))))
			 val
		       (list 'quote val)))))
    (setq var (eval var))
    (cond ((eq (car-safe var) 'progn)
	   (while (setq var (cdr var))
	     (prin1 (car var))
	     (princ "\n")
	     (if (cdr var) (princ "  "))))
	  (var
	   (prin1 var))))
  (if var (princ "\n")))

(defun sql-save-current-options ()
  "Saves the current settings of the `Options' menu to your `.sql-mode' file."
  (interactive)
  (message "Saving current options to ~/.sql-mode...")
  (let ((output-buffer (find-file-noselect
			(expand-file-name
			 (concat "~" init-file-user "/.sql-mode"))))
	output-marker)
    (save-excursion
      (set-buffer output-buffer)

      ;; Find and delete the previously saved data, and position to write.

      (goto-char (point-min))
      (if (re-search-forward "^;*\n;; SQL Options Menu Settings ;;\n"
			     nil 'move)
	  (let ((p (match-beginning 0)))
	    (goto-char p)
	    (or (re-search-forward
		 "^;; END Options Menu Settings ;;\n;*\n"
		 nil t)
		(error "can't find END of saved state in .sql-mode"))
	    (delete-region p (match-end 0)))
	(goto-char (point-max))
	(insert "\n"))
      (setq output-marker (point-marker))

    ;; run with current-buffer unchanged so that variables are evaluated in
    ;; the current context, instead of in the context of the ".sql-mode"
    ;; buffer.

    (let ((print-readably t)
	  (print-escape-newlines t)
	  (standard-output output-marker))
      (princ ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n")
      (princ ";; SQL Options Menu Settings ;;\n") 
      (princ ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n")
      (princ "(cond\n")
      (princ " ((and (string-match \"Lucid\" emacs-version)\n")
      (princ "       (boundp 'emacs-major-version)\n")
      (princ "       (= emacs-major-version 19)\n")
      (princ "       (>= emacs-minor-version 10))\n")
      (mapcar 'sql-flatten-form sql-options-menu-saved-forms)
      (princ "  ))\n")
      (princ ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n")
      (princ ";; END Options Menu Settings ;;\n") 
      (princ ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n"))
    (set-marker output-marker nil)
    (save-excursion
      (set-buffer output-buffer)
      (save-buffer)))
  (popup-dialog-box
   '("
I have added some lines to the file ~/.sql-mode in order
to save the options you have selected.

Some changes may not take affect until you re-start emacs.
"
     ["OK"	'sql-no-op	t]))
  (message "Saving current options to ~/.sql-mode... done")))

(defun sql-determine-video-type ()
  "Guess at a value for the variable `sql-video-type'.

This function does much better under XEmacs as opposed to FSF emacs."
  (let ((color (if sql-lucid (x-color-display-p)
		 (x-display-color-p))))
    (setq sql-video-type
	  (if (not color)
	      'monochrome
	    (cond
	     ((and sql-lucid (< emacs-minor-version 12))
	      (if (string-equal (pixel-name (face-background 'default))
				"black")
		  'inverse
		'regular))
	     (sql-lucid
	      'regular)
	     (t
	      'regular))))))

(defun sql-mode-font-lock-hook ()
  (cond ((or (eq major-mode 'sql-mode)
	     (eq major-mode 'sql-batch-mode))
	 (setq font-lock-keywords sql-mode-font-lock-keywords))
	((eq major-mode 'sql-interactive-mode)
	 (setq font-lock-keywords
	       (append sql-mode-font-lock-keywords
		       sql-interactive-mode-font-lock-keywords)))
	((eq major-mode 'sql-results-mode)
	 (setq font-lock-keywords sql-results-mode-font-lock-keywords))
	(t
	 nil)))

(defun sql-setup-font-lock ()
  "Set up faces etc. for font-lock-mode."
  (if (not sql-font-lock-buffers)
      ()
    (require 'font-lock)
    (or sql-video-type (sql-determine-video-type))
    (copy-face 'default 'sql-query-face)
    (copy-face 'default 'sql-set-face)
    (copy-face 'default 'sql-special-face)
    (copy-face 'default 'sql-conjunction-face)
    (copy-face 'default 'sql-sysadm-face)
    (copy-face 'default 'sql-aggregate-face)
    (copy-face 'default 'sql-prompt-face)
    (make-face-bold 'sql-prompt-face)
    (copy-face 'default 'sql-results-face)
    (copy-face 'default 'sql-changed-area-face)
    (copy-face 'default 'sql-changed-line-face)
    (copy-face 'default 'sql-invisible-face)
    (cond
     ((eq sql-video-type 'regular)
      (set-face-foreground 'sql-query-face "indianred")
      (set-face-foreground 'sql-set-face "mediumseagreen")
      (set-face-foreground 'sql-special-face "magenta")
      (set-face-foreground 'sql-conjunction-face "blue")
      (set-face-foreground 'sql-sysadm-face "red")
      (set-face-foreground 'sql-aggregate-face "forestgreen")
      (set-face-foreground 'sql-prompt-face "darkgreen")
      (set-face-foreground 'sql-results-face "blue")
      (set-face-foreground 'sql-changed-area-face "red")
      (set-face-background 'sql-changed-area-face "grey75")
      (set-face-foreground 'sql-changed-line-face "blue")
      (set-face-background 'sql-changed-line-face "grey75")
      (set-face-foreground 'sql-invisible-face (face-background 'default)))
	 
     ((eq sql-video-type 'inverse)
      (set-face-foreground 'sql-query-face "#00ffff")
      (set-face-foreground 'sql-set-face "yellow")
      (set-face-foreground 'sql-special-face "green")
      (set-face-foreground 'sql-conjunction-face "magenta")
      (set-face-foreground 'sql-sysadm-face "red")
      (set-face-foreground 'sql-aggregate-face "orange")
      (set-face-foreground 'sql-prompt-face "tan")
      (set-face-foreground 'sql-results-face "yellow")
      (set-face-foreground 'sql-changed-area-face "red")
      (set-face-background 'sql-changed-area-face "grey35")
      (set-face-foreground 'sql-changed-line-face "yellow")
      (set-face-background 'sql-changed-line-face "grey35")
      (set-face-foreground 'sql-invisible-face (face-background 'default)))

     ((eq sql-video-type 'monochrome)
      (make-face-italic 'sql-query-face)
      (make-face-bold-italic 'sql-set-face)
      (set-face-underline-p 'sql-special-face t)
      (make-face-italic 'sql-conjunction-face)
      (make-face-bold 'sql-sysadm-face)
;	  (set-face-foreground 'sql-aggregate-face "orange")
;	  (set-face-foreground 'sql-prompt-face "tan")
;	  (set-face-foreground 'sql-results-face "yellow")
      (set-face-underline-p 'sql-changed-area-face t)
      (if sql-xemacs-19-12
	  (make-face-italic 'sql-changed-area-face)
	(make-face-italic 'sql-changed-area-face t))
      (set-face-underline-p 'sql-changed-line-face t)
      (set-face-foreground 'sql-invisible-face (face-background 'default))))

    (make-face-bold 'sql-prompt-face)

    (defvar sql-mode-font-lock-keywords
      '(("\\<\\(select\\|from\\|where\\|tran\\|transaction\\|commit\\|group\\|exec\\|execute\\|readtext\\|rollback\\|compute\\|union\\|by\\|order\\|having\\|SELECT\\|FROM\\|WHERE\\|TRAN\\|TRANSACTION\\|COMMIT\\|GROUP\\|EXEC\\|EXECUTE\\|READTEXT\\|ROLLBACK\\|COMPUTE\\|UNION\\|BY\\|ORDER\\|HAVING\\)\\>" 1 sql-query-face)
	("\\<\\(set\\|update\\|delete\\|insert\\|into\\|writetext\\|values\\|SET\\|UPDATE\\|DELETE\\|INSERT\\|INTO\\|WRITETEXT\\|VALUES\\)\\>" 1 sql-set-face)
	("\\<\\(go\\|use\\|null\\|GO\\|USE\\|NULL\\)\\>" 1 sql-special-face)
	("\\<\\(begin\\|end\\|else\\|if\\|goto\\|break\\|continue\\|raiserror\\|waitfor\\|and\\|or\\|not\\|in\\|is\\|declare\\|print\\|return\\|exists\\|like\\|BEGIN\\|END\\|ELSE\\|IF\\|GOTO\\|BREAK\\|CONTINUE\\|RAISERROR\\|WAITFOR\\|AND\\|OR\\|NOT\\|IN\\|IS\\|DECLARE\\|PRINT\\|RETURN\\|EXISTS\\|LIKE\\)\\>" 1 sql-conjunction-face)
	("\\<\\(sum\\|avg\\|count\\|max\\|min\\|all\\|distinct\\|SUM\\|AVG\\|COUNT\\|MAX\\|MIN\\|ALL\\|DISTINCT\\)\\>" 1 sql-aggregate-face)
	("\\<\\(to\\|alter\\|table\\|database\\|create\\|disk\\|nonclustered\\|reconfigure\\|revoke\\|override\\|procedure\\|proc\\|checkpoint\\|dump\\|drop\\|index\\|fillfactor\\|rule\\|shutdown\\|tape\\|view\\|truncate\\|kill\\|load\\|clustered\\|dbcc\\|grant\\|as\\|with\\|nowait\\|no_log\\|refit\\|reinit\\|init\\|mirror\\|unmirror\\|remirror\\|default\\|sp_[a-zA-Z]*\\|statistics\\|TO\\|ALTER\\|TABLE\\|DATABASE\\|CREATE\\|DISK\\|NONCLUSTERED\\|RECONFIGURE\\|REVOKE\\|OVERRIDE\\|PROCEDURE\\|PROC\\|CHECKPOINT\\|DUMP\\|DROP\\|INDEX\\|FILLFACTOR\\|RULE\\|SHUTDOWN\\|TAPE\\|VIEW\\|TRUNCATE\\|KILL\\|LOAD\\|CLUSTERED\\|DBCC\\|GRANT\\|AS\\|WITH\\|NOWAIT\\|NO_LOG\\|REFIT\\|REINIT\\|INIT\\|MIRROR\\|UNMIRROR\\|REMIRROR\\|DEFAULT\\|SP_[a-zA-Z]*\\|STATISTICS\\)\\>" 1 sql-sysadm-face)))
	
    (defvar sql-interactive-mode-font-lock-keywords
      '(("^[0-9][0-9]*> " 0 sql-prompt-face)
	("fsql-[0-9]*) " 0 sql-prompt-face)))
	
    (defvar sql-results-mode-font-lock-keywords
      (if sql-column-separator
	  '(("\|" 0 sql-invisible-face))
	nil))
    
    (or (member 'sql-mode-font-lock-hook font-lock-mode-hook)
	(add-hook 'font-lock-mode-hook 'sql-mode-font-lock-hook))))

(defun sql-display-info ()
  "Insert info about the current buffer."
  (cond
   ((eq major-mode 'sql-mode)
    (princ (format "Info on SQL Mode buffer %s\n\n"
		   (buffer-name (current-buffer))))
    (princ (format "File:               %s\n" (buffer-file-name)))
    (if (buffer-live-p sql-evaluation-buffer)
	  (princ (format "Evaluation Buffer:  %s\n"
			 (buffer-name sql-evaluation-buffer))))
    )
   ((eq major-mode 'sql-batch-mode)
    (princ (format "Info on SQL Batch Mode buffer %s\n\n"
		   (buffer-name (current-buffer))))
    (princ (format "Server:             %s\n" sql-server))
    (princ (format "User:               %s\n" sql-user))
    (if (null sql-secure-passwords)
	      (princ (format "Password:           %s\n" sql-password)))
    (if sql-database
	(princ (format "Database:           %s\n" sql-database)))
    (princ (format "History:            %d\n" (length sql-history)))
    (princ (format "Global History:     %d\n" (length sql-global-history)))
    (princ "\nCached\n")
    (princ (format " Tables:            %s\n" (if sql-table-list
						  "Yes" "No")))
    (princ (format " Columns:           For %d Tables\n"
		   (length sql-column-list)))
    (princ (format " Values:            For %d Columns\n"
		   (length sql-value-list)))
    (princ (format " Stored Procedures: %s\n" (if sql-stored-procedure-list
						  "Yes" "No")))
    (princ (format " Databases:         %s\n" (if sql-database-list
						  "Yes" "No")))
    (princ (format " Users:             %s\n" (if sql-user-list
						  "Yes" "No"))))
   ((eq major-mode 'sql-interactive-mode)
    (princ (format "Info on SQL Interactive Mode buffer %s\n\n"
		   (buffer-name (current-buffer))))
    (princ (format "Server:         %s\n" sql-server))
    (princ (format "User:           %s\n" sql-user))
    (if (null sql-secure-passwords)
	      (princ (format "Password:       %s\n" sql-password))))
   ((eq major-mode 'sql-results-mode)
    (princ (format "Info on SQL Results Mode buffer %s\n\n"
		   (buffer-name (current-buffer))))
    (princ (format "Server:         %s\n" sql-server))
    (princ (format "User:           %s\n\n" sql-user))
    (princ (format "History:        %d\n" (length sql-history)))
    (princ (format "Global History: %d\n" (length sql-global-history))))
   (t nil)))
	
(defun sql-current-buffer-info ()
  "Display info about the current buffer in a temporary buffer."
  (interactive)
  (let ((conf (current-window-configuration)))
    (with-output-to-temp-buffer "*Info*"
      (sql-display-info))
    (sql-restore-window-config
     conf
     "Press `s' to save buffer information, anything else to flush")))

(defun sql-exit-sql-mode ()
  "Exit the sql-mode buffer, and the matching buffer if it exists."
  (interactive)
  (let ((this-buffer (current-buffer))
	(matching-buffer sql-matching-buffer))
;    (sql-maybe-delete-frame)
    (kill-buffer this-buffer)
    (delete-other-windows)
    (if (buffer-live-p matching-buffer)
;	(progn
;	  (set-buffer matching-buffer)
;	  (let ((frame (car (frames-of-buffer matching-buffer))))
	;    (and frame (sql-maybe-delete-frame frame)))
	  (kill-buffer matching-buffer))))

(defun sql-about-sql-mode ()
  "Display a popup dialog box displaying information on sql-mode.el"
  (interactive)
  (let ((prompt "
SQL Mode version 0.922 (beta)
Copyright (c) 1994, 1995, 1996 Peter D. Pezaris


    SQL Mode comes with ABSOLUTELY NO WARRANTY.
    Type M-x describe-no-warranty for details.

    SQL Mode is beta release software.  Feel free         
    to distribute it, but USE AT YOUR OWN RISK.

    Type C-c h for general help on SQL Mode.
    Type C-h m for help on the current mode.
"))
    (if sql-lucid
	(popup-dialog-box (list prompt ["OK" 'sql-no-op t]))
      (x-popup-dialog t (cons prompt '(("OK" . t)))))))

(defun sql-no-op ()
  (interactive)
  nil)

(defun sql-y-or-n-p-maybe-dialog-box (prompt)
  (if sql-lucid
      (y-or-n-p-maybe-dialog-box prompt)
    (x-popup-dialog t (cons prompt '(("YES" . t) ("NO" . nil))))))

(defun sql-request-latest-version ()
  "Submit via mail a request for the latest version of sql-mode."
  (interactive)
  (require 'reporter)
  (and
   (sql-y-or-n-p-maybe-dialog-box 
    "Do you want to request the latest version of SQL Mode? ")
   (reporter-submit-bug-report
    sql-mode-help-address
    (concat "SQL Mode Version " sql-mode-version)
    (list 'major-mode)
    nil
    nil
    "Please send me the latest version of SQL Mode..")))

(defun sql-submit-enhancement-request ()
  "Submit via mail an enhancement request for sql-mode."
  (interactive)
  (require 'reporter)
  (if (or (eq major-mode 'sql-mode)
	  (eq major-mode 'sql-batch-mode)
	  (eq major-mode 'sql-interactive-mode)
	  (eq major-mode 'sql-results-mode))
      (and
       (sql-y-or-n-p-maybe-dialog-box 
	"Do you want to submit an enhancement request for SQL Mode? ")
       (reporter-submit-bug-report
	sql-mode-help-address
	(concat "SQL Mode Version " sql-mode-version)
	(list 'major-mode)
	nil
	nil
	"Wouldn't it be nice if..."))
    (error
     "You must be in a sql related mode to submit an enhancement request")))

(defun sql-submit-bug-report ()
  "Submit via mail a bug report on sql-mode."
  (interactive)
  (require 'reporter)
  (if (or (eq major-mode 'sql-mode)
	  (eq major-mode 'sql-batch-mode)
	  (eq major-mode 'sql-interactive-mode)
	  (eq major-mode 'sql-results-mode))
      (and
       (sql-y-or-n-p-maybe-dialog-box 
	"Do you want to submit a bug report on SQL Mode? ")
       (reporter-submit-bug-report
	sql-mode-help-address
	(concat "SQL Mode Version " sql-mode-version)
	(list
	 'major-mode
	 'sql-command
	 'sql-batch-command-switches
	 'sql-interactive-command-switches
	 'sql-require-final-go
	 'sql-secure-passwords
	 'sql-abbrev-mode
	 'sql-video-type
	 'sql-resize-results-frames
	 'sql-results-frame-width
	 'sql-results-frame-height
	 'sql-max-frame-width
	 'sql-scroll-overlap
	 'sql-save-all-results
	 'sql-results-in-new-frame
	 'sql-comment-regions-by-line
	 'sql-font-lock-buffers
	 'sql-mark-changes
	 'sql-association-mode-no-create
	 'sql-require-where
	 'sql-require-where-regexp 
	 'sql-confirm-changes
	 'sql-confirm-changes-regexp
	 'sql-history-length
	 'sql-global-history-length
	 'sql-deactivate-region
	 'sql-indent-after-newline
	 'sql-inhibit-startup-message
	 'sql-add-to-menu-bar
	 'sql-intersperse-headers
	 'sql-use-toolbar
	 'sql-use-top-ten-toolbar
	 'sql-use-big-menus
	 'sql-stay-in-batch-buffer)))
    (error "You must be in a sql related mode to submit a bug report.")))

(defun sql-add-menus ()
  "Add menu items under the menu `sql-parent-menu'."
  (if (and sql-add-to-menu-bar
	   (boundp 'sql-association-alist)
	   sql-association-alist)
      (progn
	(setq sql-batch-menu
	  (and (boundp 'sql-association-alist)
	       sql-association-alist
	       (list
		(append
		 (list "Batch")
		 (sql-make-association-menu sql-association-alist
						 'sql-batch-mode)))))

	(setq sql-interactive-menu
	      (and (boundp 'sql-association-alist)
		   sql-association-alist
		   (list
		    (append
		     (list "Interactive")
		     (sql-make-association-menu sql-association-alist
						'sql-interactive-mode)))))

	(setq sql-big-menu
	      (append sql-actions-menu
		      sql-execute-menu
		      sql-history-menu
		      sql-batch-menu
		      sql-interactive-menu
		      sql-settings-menu
		      sql-options-menu
		      sql-font-lock-menu
;		      (if (< emacs-minor-version 12)
;			  (list (list "Buffers"))
;			(list
;			 '("Buffers"
;			  :filter buffers-menu-filter
;			  ["List All Buffers" list-buffers t]
;			  "--!here"	; anything after this will be nuked
;			  )))
		      (list ["XEmacs" sql-use-emacs-menu t])
		      (list nil)
		      sql-help-menu))

	(if sql-lucid
	    (progn
	      (and (car (find-menu-item current-menubar sql-parent-menu))
		   (add-menu-item sql-parent-menu "------------" nil nil))
	      (add-menu-item sql-parent-menu "SQL Batch Mode" 'sql-batch-mode t)
	      (add-menu sql-parent-menu "Use Association" 
			(cdr (sql-make-popup-menu 'sql-batch-mode)))
	      (add-menu-item sql-parent-menu "-----------" nil nil)
	      (add-menu-item sql-parent-menu "SQL Interactive Mode" 
			     'sql-interactive-mode t)
	      (add-menu sql-parent-menu 
			"Use Association " 
			(cdr (sql-make-popup-menu 'sql-interactive-mode))))
	  (easy-menu-define
	   sql-utilities-menu (list global-map) "SQL"
	   (cons "SQL-assoc"
		 (list
		  ["SQL Batch Mode" 'sql-batch-mode t]
		  (cons "Use Association"
			(cdr (sql-make-popup-menu 'sql-batch-mode)))
		  "----"
		  ["SQL Interactive Mode" 'sql-interactive-mode t]
		  (cons "Use Association "
			(cdr (sql-make-popup-menu 'sql-interactive-mode))))))))
    nil))

(defun sql-add-minor-modes ()
  "Add some minor modes to the variable `minor-mode-alist'."
  (or (assq 'sql-query-in-progress minor-mode-alist)
      (setq minor-mode-alist (cons '(sql-query-in-progress " Querying")
				   minor-mode-alist)))
  (or (assq 'sql-results-view-mode minor-mode-alist)
      (setq minor-mode-alist (cons '(sql-results-view-mode " View")
				   minor-mode-alist)))
  (or (assq 'sql-results-edit-mode minor-mode-alist)
      (setq minor-mode-alist (cons '(sql-results-edit-mode " Edit")
				   minor-mode-alist))))

(defun sql-display-getting-comments-info (the-file the-directory)
  "Display information on how to properly set up the comments file.
THE-FILE is the file that needs to be moved, and THE-DIRECTORY is where
it needs to be moved to."
  (interactive)
  (with-output-to-temp-buffer "*Help*"
    (princ (format "The file %s has not been properly set up\n" the-file))
    (princ "to display appropriate help information.\n\n")
    (princ "In order to get the help system to work, you should copy\n\n")
    (princ (format "            the file: %s\n" the-file))
    (princ (format "  into the directory: %s\n\n" the-directory))
    (princ "And then re-invoke this help command.\n\n")
    (princ (format "You should have received the file %s\n" the-file))
    (princ "with the sql-mode distribution.  If you did not, you can\n")
    (princ "request it by invoking the command:\n\n")
    (princ "  M-x sql-request-latest-version")))

(defun sql-display-comments ()
  "Display the sql-mode.el file in a temporary buffer."
  (interactive)
  (setq sql-old-window-configuration (current-window-configuration))
  (let ((file-name (expand-file-name "SQL-MODE-README" data-directory)))
    (if (not (file-readable-p file-name))
	(progn
	  (sql-display-getting-comments-info "SQL-MODE-README" data-directory)
	  (error "Could not find %s" file-name))
      (bury-buffer (get-buffer "*Help*"))
      (find-file file-name)
      (view-mode)
      (delete-other-windows)
      (goto-char (point-min))
      (toggle-read-only 0)
      (local-set-key "q" 'sql-quit-reading-comments)
      (message "Type SPACE to scroll, q to quit."))))

(defun sql-quit-reading-comments ()
  "Kill the current buffer and restore the old winodow configuration."
  (interactive)
  (kill-buffer nil)
  (set-window-configuration sql-old-window-configuration))

(defun sql-cleanup-info ()
  "Clean up the display of information."
  (kill-line)
  (delete-region (point) (point-min))
  (forward-line 1)
  (delete-char 3)
  (forward-line 1)
  (kill-line)
  (search-forward ";;;;;")
  (beginning-of-line)
  (delete-region (point) (point-max))
  (goto-char (point-min))
  (forward-line 1)
  (center-line)
  (while (eq 0 (forward-line 1))
    (and (looking-at ";;") (delete-char 2)))
  (goto-char (point-min)))

(defun sql-advanced-usage-info ()
  "Display the information on advanced usage of SQL Mode."
  (interactive)
  (sql-display-comments)
  (re-search-forward "Advanced Usage:")
  (beginning-of-line)
  (forward-line -1)
  (set-window-start nil (point))
  (sql-cleanup-info)
  (toggle-read-only 1)
  (set-buffer-modified-p nil))

(defun sql-basic-usage-info ()
  "Display the information on basic usage of SQL Mode."
  (interactive)
  (sql-display-comments)
  (re-search-forward "Basic Usage:")
  (beginning-of-line)
  (forward-line -1)
  (set-window-start nil (point))
  (sql-cleanup-info)
  (toggle-read-only 1)
  (set-buffer-modified-p nil))
      
(defun sql-customization-info ()
  "Display the information on customizing SQL Mode."
  (interactive)
  (sql-display-comments)
  (re-search-forward "Customization:")
  (beginning-of-line)
  (forward-line -1)
  (set-window-start nil (point))
  (sql-cleanup-info)
  (toggle-read-only 1)
  (set-buffer-modified-p nil))

(defun sql-goto-matching-buffer (&optional no-error)
  "Go to the matching buffer of the current one, if it exists."
  (if (buffer-live-p sql-matching-buffer)
      (set-buffer sql-matching-buffer)
    (if (not no-error)
	(error "There is no matching buffer for current buffer.")
      nil)))

(defun sql-find-next-error-line (&optional number)
  "Parse the current results buffer, searching for the next error.
Return the line number on which the error occurs."
  (let* ((times (or number 1))
	 (success (if (< times 0)
		      (re-search-backward sql-error-regexp nil t (1+ (- times)))
		    (re-search-forward sql-error-regexp nil t times))))
    (if (not success)
	(error "No more errors.")
      (goto-char (match-end 0))
      (let ((line (string-to-int (current-word))))
	(set-window-start (get-buffer-window (current-buffer))
			  (match-beginning 0))
	(setq sql-current-error-point (match-end 0))
	(message "Error found on line %d." line)
	line))))

(defun sql-next-error (&optional number)
  "Visit next evaluation error message and corresponding line of SQL code.
This operates on the output from the \\[sql-evaluate-buffer] command."
  (interactive)
  (if (eq major-mode 'sql-batch-mode)
      (sql-goto-matching-buffer))
  (goto-char (or sql-current-error-point (point-min)))
  (let ((this-error-line (sql-find-next-error-line number)))
    (sql-goto-matching-buffer)
    (goto-line this-error-line)))

(defun sql-previous-error ()
  "Visit previous evaluation error message and corresponding line of SQL code.
This operates on the output from the \\[sql-evaluate-buffer] command."
  (interactive)
  (sql-next-error -1))

(defun sql-describe-keybindings ()
  "Display the current SQL-specific keybindings that are in effect."
  (interactive)
  (let ((prefix (vector '(control c))))
    (with-output-to-temp-buffer (gettext "*Help*")
      (princ (gettext "Key bindings starting with "))
      (princ (key-description prefix))
      (princ ":\n\n")
      (describe-bindings-1 prefix nil))))

;(defun sql-news-info ()
;  "Display the file NEWS that comes with the SQL Mode distribution."
;  ())

(defun sql-masthead-info ()
  "Display the masthead information of SQL Mode."
  (interactive)
  (sql-display-comments)
  (goto-char (point-min)))

(defun sql-bug-report-info ()
  "Display the information on how to submit a bug report for SQL Mode."
  (interactive)
  (sql-display-comments)
  (re-search-forward "Bug Reports:")
  (beginning-of-line)
  (forward-line -1)
  (set-window-start nil (point))
  (sql-cleanup-info)
  (toggle-read-only 1)
  (set-buffer-modified-p nil))

(defun sql-help-quit ()
  (interactive)
  nil)

(define-function 'sql-help 'sql-help-for-help)

(defun sql-help-for-help ()
  "You have typed \\[sql-help], the SQL Help character.  Type a Help option:
\(Type \\<sql-help-map>\\[sql-help-quit] to exit the Help command.)

a  sql-advanced-usage-info     Show information on advanced usage concepts.
b  sql-basic-usage-info        Show information on basic usage concepts.
c  sql-customization-info      Show information on customization.
i  sql-current-buffer-info     Show the current buffer's information.
k  sql-describe-keybindings    Show the SQL-specific keybindings in effect.
m  sql-masthead-info           Show the author and maintainer information.
n  sql-next-error              Go to the next line which contains an error.
p  sql-previous-error          Go to the previous line which contains an error.
r  sql-bug-report-info         Show information on how to report bugs.
s  sql-about-sql-mode          Show the version and other data about SQL Mode."
  (interactive)
  (let ((help-key (copy-event last-command-event))
	event char)
    (message "A B C I K M N P R S.  Type h or ? again for more help: ")
    (setq event (next-command-event)
	  char (or (event-to-character event) event))
    (if (or (equal char help-key) (equal char ?h)
	    (equal char ??) (button-event-p event))
	(save-window-excursion
	  (switch-to-buffer (gettext "*Help*"))
	  (delete-other-windows)
	  (erase-buffer)
	  (insert (documentation 'sql-help))
	  (goto-char (point-min))
	  (while (or (equal char help-key)
		     (memq char '(?h ?? ?\C-v ?\ ?\177 ?\M-v))
		     (eventp char))
	    (if (memq char '(?\C-v ?\ ))
		(scroll-up))
	    (if (memq char '(?\177 ?\M-v))
		(scroll-down))
	    (message (if (pos-visible-in-window-p (point-max))
			 "A B C I K M N P R S: "
		       "A B C I K M N P R S or Space to scroll: "))
	    (let ((cursor-in-echo-area t))
	      (setq event (next-command-event event)
		    char (or (event-to-character event) event))))))
    (let ((defn (or (lookup-key sql-help-map (vector event))
		    (and (numberp char)
			 (lookup-key sql-help-map
				     (make-string 1 (downcase char)))))))
      (message nil)
      (if defn
	  (call-interactively defn)
	(ding)))))

(defun sql-isearch-end ()
  "Function to run when isearch terminates."
  (remove-hook 'post-command-hook 'sql-horizontal-recenter)
  (sql-horizontal-recenter))

(defun sql-isearch-begin ()
  "Function to run when isearch begins."
  (sql-horizontal-recenter)
  (add-hook 'post-command-hook 'sql-horizontal-recenter))

;(defun sql-magic-update ()
;  "Mouse-based update of a table."
;  (interactive)
;  (let ((table (sql-get-table-name)))
;    (erase-buffer)
;    (insert "update " table " set ")
;    (setq sql-magic-update-in-progress t)
;    (message "Click on columns to change")))

;(defun sql-magic-yank-under-point (event)
;  (interactive "e")
;  (let ((the-word nil))
;    (save-window-excursion
;      (mouse-set-point event)
;      (setq the-word (current-word)))
;;    (sql-goto-batch-buffer)
;    (cond
;	; sitting at empty command line
;     ((eq (current-column) 0)
;      (insert "select "))
;       ; on same line as SELECT or ORDER BY, but other words already inserted
;     ((save-excursion (re-search-backward "select .+\\|order by .+\\|update .+" 
;					  (save-excursion (beginning-of-line)
;							  (point)) t))
;      (and (eq (preceding-char) ? )
;	   (backward-delete-char 1))
;      (insert ", "))
;       ; Otherwise
;     (t
;      (or (eq (preceding-char) ? )	; if preceding character = space
;	  (insert " "))))
;    (insert the-word " ")))

;(defun sql-current-value (table event)
;  (let* ((column-list (cdr (assoc table sql-column-list)))
;	 (column nil)
;	 (width nil))
;    (save-window-excursion
;      (mouse-set-point event)
;      (let ((cc (current-column)))
;	(search-backward "----" nil t)
;	(forward-line -1)
;	(goto-column cc)
;;	(beginning-of-w
;	(setq column (current-word))
;	(setq ))
;	 )))



;;	     (no-quotes (or (string-equal "NULL" the-word)
;			    (string-match "\(.*\)" the-word)
;			    (eq 0 (cdr (assoc column column-list))))))
;	(insert column
;		" "
;		(or sql-preferred-operator "")
;		(if no-quotes "" "\"")
;		the-word
;		(if no-quotes "" "\"")
;		" "))))

(defun sql-magic-yank-under-point (event)
  (interactive "e")
  (let ((the-word nil))
    (save-window-excursion
      (mouse-set-point event)
      (setq the-word (current-word)))
    (let ((context (sql-get-completion-context))
	  (table (sql-get-table-name)))
      (cond
;       ((eq context 'keyword)
;	(let ((beg (save-excursion (beginning-of-line) (point))))
;	  (if (not (re-search-backwards "where" beg t))
;	      (progn
;		(in
;	)
;       ((eq context 'table)
;	)
       ((eq context 'column)
	(let* ((column-list (cdr (assoc table sql-column-list)))
	       (exists (assoc the-word column-list)))
	  (if (or exists (null column-list))
	      (insert the-word " ")
	    (let* ((column (save-window-excursion
			     (mouse-set-point event)
			     (let ((cc (current-column)))
			       (search-backward "----" nil t)
			       (forward-line -1)
			       (goto-column cc)
			       (current-word))))
		   (no-quotes (or (string-equal "NULL" the-word)
				  (string-match "\(.*\)" the-word)
				  (string-match "\"\\|'" the-word)
				  (string-equal "null" the-word)
				  (eq 0 (cdr (assoc column column-list))))))
	      (insert column
		      " "
		      (or sql-preferred-operator "")
		      (if no-quotes "" "\"")
		      the-word
		      (if no-quotes "" "\"")
		      " "))))
	)
;       ((eq context 'stored-procedure)
;	)
;       ((eq context 'database)
;	)
;       ((eq context 'user)
;	)
       ((eq context 'value)
	(let* ((column (sql-previous-word 2))
	       (column-list (cdr (assoc table sql-column-list)))
	       (no-quotes (or (string-equal "NULL" the-word)
			      (string-match "\(.*\)" the-word)
			      (string-match "\"\\|'" the-word)
			      (string-equal "null" the-word)
			      (eq 0 (cdr (assoc column column-list))))))
	  (insert (if no-quotes "" "\"")
		  the-word
		  (if no-quotes "" "\"")
		  " ")))
       ((eq context 'operator)
	(insert (or sql-preferred-operator "") the-word " ")
	)
       (t
	(insert the-word " "))))))

;(defun sql-magic-yank-under-point-2 (event)
;  (interactive "@e")
;  (mouse-set-point event)
;  (let ((the-word (current-word))
;	(next-line-dash (save-excursion
;			  (forward-line 1)
;			  (beginning-of-line)
;			  (looking-at " *-*")))
;	(insert-word nil))
;    (if next-line-dash
;	(sql-do-some-stuff)
;      (

(defun sql-string-is-numberp (string)
  (cond
   ((eq 0 (length string))
    nil)
   ((eq 1 (length string))
    (or (string-equal string "0") (not (eq (string-to-int string) 0))))
   (t
    (let ((char (substring string 0 1)))
      (and (or (string-equal char "0") (not (eq (string-to-int char) 0)))
	   (sql-string-is-numberp (substring string 1)))))))

;(defun sql-yank-under-point (event)
;  (interactive "@e")
;  (mouse-set-point event)
;  (let ((the-word (current-word)))
;    (sql-goto-batch-buffer)
;    (insert the-word)))

(defun sql-yank-under-point (event)
  (interactive "e")
  (let ((old-point (point))
	(the-word nil))
    (save-window-excursion
      (mouse-set-point event)
      (setq the-word (current-word)))
    (goto-char old-point)
    (insert the-word)))

(defun sql-insert-row (&optional update-interactively)
  "Insert a row into the current database.

If optional UPDATE-INTERACTIVELY is non-nil, update the buffer as values
are entered."
  (interactive)
  (sql-goto-batch-buffer)
  (setq sql-completion-saved-buffer (current-buffer))
  (sql-goto-results-buffer)
  (let ((table (completing-read "Insert into table: " 'sql-complete-table-list))
	(insert-string nil))
    (setq sql-last-table table)
    (sql-goto-batch-buffer)
    (and update-interactively (erase-buffer))
    (if update-interactively
	(insert "insert into " table " values (")
      (setq insert-string (concat "insert into " table " values (")))
    (or (assoc table sql-column-list)
	(sql-get-columns table))
    (let ((columns (reverse (cdr (assoc table sql-column-list)))))
      (setq sql-completion-saved-table table)
      (while columns
	(setq sql-completion-saved-column (car (car columns)))
	(let* ((prompt (concat "Insert into " table ": "
			       (car (car columns)) " = "))
	       (value (completing-read prompt 'sql-complete-value-list))
	       (value-string (if (or (eq (cdr (car columns)) 0)
				     (string-match "\"\\|'" value)
				     (string-equal "null" value)
				     (string-match "\(.*\)" value)
				     (string-equal value "NULL"))
				 value
			       (concat "\"" value "\""))))
	  (if update-interactively
	      (insert (if (numberp value-string)
			  (int-to-string value-string)
			value-string)
		      ", ")
	    (setq insert-string (concat insert-string value-string ", ")))
	  (setq columns (cdr columns))))
      (if update-interactively
	  (progn
	    (backward-delete-char 2)
	    (insert ")\n"))
	(setq insert-string
	      (substring insert-string 0 (- (length insert-string) 2)))
	(setq insert-string (concat insert-string ")\n"))
	(or update-interactively (erase-buffer))
	(insert insert-string)
	(message (substitute-command-keys
		  "Type \\[sql-evaluate-buffer] to insert the row."))))))

(defun sql-cut-row ()
  "Cut the row that point is currently on.

This function will create a SQL statement which, when evaluated, will
have the effect of deleting the current row.

The row that is deleted is copied to the SQL clipboard."
  (interactive)
  (let ((append (eq last-command 'sql-cut-row)))
    (or append (setq sql-clipboard-rows nil))
    (sql-delete-row nil append t)
;  (sql-goto-results-buffer)
    )
  (message (substitute-command-keys
	    "Type \\[sql-evaluate-buffer] to delete the row.")))
  
(defun sql-delete-row (&optional in-table append update-clipboard)
  "Delete the row that point is currently on.

This function will create a SQL statement which, when evaluated, will
have the effect of deleting the current row."
  (interactive)
  (or (eq major-mode 'sql-results-mode)
      (error "You must be in a sql-results-mode buffer."))
  (sql-goto-batch-buffer)
  (setq sql-completion-saved-buffer (current-buffer))
  (let ((default-table (sql-get-table-name)))
    (sql-goto-results-buffer)
    (let ((table (or in-table
		     (completing-read (format "Delete from table (default %s): "
					      default-table)
				      'sql-complete-table-list))))
      (and (string-equal table "") (setq table default-table))
      (and (string-equal table "") (sql-abort-update "Invalid table selection."))
      (setq sql-last-table table)
      (sql-goto-batch-buffer)
      (or (assoc table sql-column-list)
	  (sql-get-columns table))
      (sql-goto-results-buffer)
      (let* ((column-names (sql-build-column-names))
	     (column-widths (sql-build-column-widths))
	     (column-pairs (sql-build-column-pairs column-names column-widths))
	     (statement ""))
	(setq sql-clipboard-rows (cons column-pairs sql-clipboard-rows))
	(sql-goto-batch-buffer)
	(let ((column-list (cdr (assoc table sql-column-list))))
	  (if append
	      (goto-char (point-max))
	    (erase-buffer))
	  (insert "delete from " table "\nwhere ")
	  (while column-pairs
	    (let* ((column (car (car column-pairs)))
		   (value (cdr (car column-pairs)))
		   (no-quotes (or (string-equal "NULL" value)
				  (string-match "\(.*\)" value)
				  (string-match "\"\\|'" value)
				  (string-equal "null" value)
				  (eq 0 (cdr (assoc column column-list))))))
	      (or (eq 2 (cdr (assoc column column-list)))
		  (setq statement (concat statement column " = "
					  (if no-quotes "" "\"")
					  value
					  (if no-quotes "" "\"")
					  "\n  and ")))
	      (setq column-pairs (cdr column-pairs))))
	  (insert statement)
	  (backward-delete-char 6)))
      (goto-char (point-min))
      (sql-goto-results-buffer)
      (sql-results-view-mode)
      (sql-goto-batch-buffer))
    (and (interactive-p)
	 (message (substitute-command-keys
		   "Type \\[sql-evaluate-buffer] to delete the row.")))))

(defun sql-delete-region (start end &optional in-table update-clipboard)
  "Delete the rows in the current region."
  (interactive "r")
  (let* ((first-line (save-excursion (goto-char start) (current-line)))
	 (last-line (save-excursion (goto-char end) (current-line)))
	 (current-line first-line))
    (sql-goto-batch-buffer)
    (setq sql-completion-saved-buffer (current-buffer))
    (let ((default-table (sql-get-table-name)))
      (sql-goto-results-buffer)
      (let* ((prompt (if update-clipboard
			 (format "Cut from table (default %s): " default-table)
		       (format "Delete from table (default %s): " default-table)))
	     (table (or in-table
			(completing-read prompt 'sql-complete-table-list))))
	(and (string-equal table "") (setq table default-table))
	(and (string-equal table "") (sql-abort-update "Invalid table selection."))
	(setq sql-last-table table)
	(sql-goto-batch-buffer)
	(erase-buffer)
	(if update-clipboard
	    (progn
	      (setq sql-clipboard-table table)
	      (setq sql-clipboard-rows nil)))
	(sql-goto-results-buffer)
	(if (or (interactive-p) update-clipboard)
	    (message "Building delete statement..."))
	(while (<= current-line last-line)
	  (goto-line current-line)
	  (sql-delete-row table t update-clipboard)
	  (goto-char (point-max))
	  (insert "\n")
	  (goto-char (point-min))
	  (sql-goto-results-buffer)
	  (setq current-line (1+ current-line)))
	(sql-goto-batch-buffer)))
    (zmacs-deactivate-region)
    (and (interactive-p)
	 (message (substitute-command-keys
		   "Type \\[sql-evaluate-buffer] to delete the rows.")))))

(defun sql-cut-region (start end &optional in-table)
  "Cut the rows in the current region."
  (interactive "r")
  (sql-delete-region start end in-table t)
  (zmacs-deactivate-region)
  (and (interactive-p)
       (message (substitute-command-keys
		 "Type \\[sql-evaluate-buffer] to delete the rows."))))

(defun sql-copy-row (&optional dont-clear-clipboard)
  "Copy the row that point is currently on."
  (interactive)
  (or (eq major-mode 'sql-results-mode)
      (error "You must be in a sql-results-mode buffer."))
  (and (interactive-p) (message "Copying row..."))
  (or dont-clear-clipboard (progn
			     (setq sql-clipboard-table nil)
			     (setq sql-clipboard-rows nil)))
  (let* ((column-names (sql-build-column-names))
	 (column-widths (sql-build-column-widths))
	 (column-pairs (sql-build-column-pairs column-names column-widths)))
    (setq sql-clipboard-rows (cons column-pairs sql-clipboard-rows)))
  (and (interactive-p) (message "Copying row... done")))

(defun sql-copy-region (start end)
  "Copy the rows in the current region."
  (interactive "r")
  (and (interactive-p) (message "Copying region..."))
  (let* ((first-line (save-excursion (goto-char start) (current-line)))
	 (last-line (save-excursion (goto-char end) (current-line)))
	 (current-line first-line))
    (setq sql-clipboard-table nil)
    (setq sql-clipboard-rows nil)
    (while (<= current-line last-line)
      (goto-line current-line)
      (sql-copy-row t)
      (setq current-line (1+ current-line)))
    (zmacs-deactivate-region)
    (and (interactive-p) (message "Copying region... done"))))

(defun sql-paste (&optional in-table)
  "Paste the rows that are in the SQL clipboard."
  (interactive)
  (sql-goto-batch-buffer)
  (setq sql-completion-saved-buffer (current-buffer))
  (let ((default-table (sql-get-table-name)))
    (sql-goto-results-buffer)
    (let* ((prompt (format "Paste into table (default %s): " default-table))
	   (table (or in-table
		      sql-clipboard-table
		      (completing-read prompt 'sql-complete-table-list)))
	   (clipboard-rows (reverse sql-clipboard-rows)))
;	 (column-pairs (car sql-clipboard-rows)))
      (and (string-equal table "") (setq table default-table))
      (and (string-equal table "") (sql-abort-update "Invalid table selection."))
      (and (interactive-p) (message "Building insert statement..."))
      (setq sql-last-table table)
      (sql-goto-batch-buffer)
      (erase-buffer)
      (let ((column-list (cdr (assoc table sql-column-list)))
	    (statement ""))
	(while clipboard-rows
	  (let ((column-pairs (car clipboard-rows)))
	    (insert "insert into " table "\nvalues (")
	    (while column-pairs
	      (let* ((column (car (car column-pairs)))
		     (value (cdr (car column-pairs)))
		     (no-quotes (or (string-equal "NULL" value)
				    (string-match "\(.*\)" value)
				    (string-match "\"\\|'" value)
				    (string-equal "null" value)
				    (eq 0 (cdr (assoc column column-list))))))
		(setq statement (concat statement (if no-quotes "" "\"")
					value
					(if no-quotes "" "\"")
					", "))
;		(message "Building insert statement... (column %s...)" column)
		(setq column-pairs (cdr column-pairs))))
	    (insert statement)
	    (backward-delete-char 2)
	    (insert ")\n"))
	  (setq clipboard-rows (cdr clipboard-rows))))
      (and (interactive-p)
	   (message (substitute-command-keys
		     "Type \\[sql-evaluate-buffer] to insert the rows."))))))

(defun sql-bcp-out-menu (table file)
  "Invoke bcp to copy TABLE out of the current database into FILE.

Display any exit status in a dialog box"
  (interactive (list (progn
		       (sql-goto-batch-buffer)
		       (setq sql-completion-saved-buffer (current-buffer))
		       (completing-read "Copy out of table: "
					'sql-complete-table-list))
		     (expand-file-name (read-file-name "Into file: "))))
  (let ((temp-buffer (get-buffer-create " SQL-TEMP"))
	(original-buffer (current-buffer))
	(table-arg (sql-create-table-arg-for-bcp table)))
    (if sql-bcp-command-switches
	(call-process sql-bcp-command nil temp-buffer nil table-arg "out" file
		      (concat "-S" sql-server)
		      (concat "-U" sql-user)
		      (concat "-P" sql-password)
		      sql-bcp-command-switches)
      (call-process sql-bcp-command nil temp-buffer nil table-arg "out" file
		    (concat "-S" sql-server)
		    (concat "-U" sql-user)
		    (concat "-P" sql-password)
		    "-c" "-t|" "-r\n"))
    (set-buffer temp-buffer)
    (and (looking-at ".*DB-LIBRARY error") (ding))
    (sql-popup-dialog-return
     (list (buffer-substring (point-min) (point-max))
	   ["OK" 'no t]))
    (kill-buffer temp-buffer)
    (set-buffer original-buffer)))

(defun sql-bcp-out (table file)
  "Invoke bcp to copy TABLE out of the current database into FILE.

Display any exit status in the echo area."
  (interactive (list (progn
		       (sql-goto-batch-buffer)
		       (setq sql-completion-saved-buffer (current-buffer))
		       (completing-read "Copy out of table: "
					'sql-complete-table-list))
		     (expand-file-name (read-file-name "Into file: "))))
  (let ((temp-buffer (get-buffer-create " SQL-TEMP"))
	(original-buffer (current-buffer))
	(table-arg (sql-create-table-arg-for-bcp table)))
    (if sql-bcp-command-switches
	(call-process sql-bcp-command nil temp-buffer nil table-arg "out" file
		      (concat "-S" sql-server)
		      (concat "-U" sql-user)
		      (concat "-P" sql-password)
		      sql-bcp-command-switches)
      (call-process sql-bcp-command nil temp-buffer nil table-arg "out" file
		    (concat "-S" sql-server)
		    (concat "-U" sql-user)
		    (concat "-P" sql-password)
		    "-c" "-t|" "-r\n"))
    (set-buffer temp-buffer)
    (goto-char (point-min))
    (replace-string "
" " ")
    (goto-char (point-min))
    (replace-string " Starting copy...  " "")
    (replace-string "   " " ")
    (and (looking-at ".*DB-LIBRARY error") (ding))
    (message (buffer-substring (point-min) (point-max)))
    (kill-buffer temp-buffer)
    (set-buffer original-buffer)))

(defun sql-fake-bcp-out ()
  (interactive)
  (or (eq major-mode 'sql-results-mode)
      (error "You must be in a sql-results-mode buffer."))
  (if (mark)
      (call-interactively 'sql-fake-bcp-out-region)
    (call-interactively 'sql-fake-bcp-out-row)))

(defun sql-fake-bcp-out-region (file start end)
  (interactive "FBCP rows into file:
r")
  (let* ((row 1)
	 (first-line (save-excursion (goto-char start) (current-line)))
	 (last-line (save-excursion (goto-char end) (current-line)))
	 (current-line first-line)
	 (names (sql-build-column-names))
	 (widths (sql-build-column-widths)))
    (message "Fake BCP rows OUT... working on row %d..." row)
    (goto-line current-line)
    (sql-fake-bcp-out-row file names widths nil)
    (setq current-line (1+ current-line))
    (setq row (1+ row))
    (while (<= current-line last-line)
      (message "Fake BCP rows OUT... working on row %d..." row)
      (goto-line current-line)
      (sql-fake-bcp-out-row file names widths t)
      (setq current-line (1+ current-line))
      (setq row (1+ row))))
  (message "Fake BCP rows OUT... done"))

(defun sql-fake-bcp-out-row (file &optional names widths append)
  "Simulate a bcp out of the row at point.  Write the contents to FILE."
  (interactive "FBCP row into file: ")
;  (sql-goto-batch-buffer)
  (and (interactive-p) (message "Fake BCP row OUT..."))
  (let* ((column-names (or names (sql-build-column-names)))
	 (column-widths (or widths (sql-build-column-widths)))
	 (column-pairs (reverse (sql-build-column-pairs column-names
							column-widths)))
	 (string nil))
    (while column-pairs
      (setq string (concat (cdr (pop column-pairs)) "|" string)))
    (setq string (substring string 0 (1- (length string))))
    (setq string (concat string "\n"))
    (let ((executing-macro t))
      (write-region string nil file append)))
  (and (interactive-p) (message "Fake BCP row OUT... done")))

(defun sql-bcp-in-menu (table file)
  "Invoke bcp to copy TABLE into the current database from FILE.

Display any exit status in a dialog box"
  (interactive (list (progn
		       (sql-goto-batch-buffer)
		       (setq sql-completion-saved-buffer (current-buffer))
		       (completing-read "Copy into table: "
					'sql-complete-table-list))
		     (expand-file-name (read-file-name "From file: "))))
  (let ((temp-buffer (get-buffer-create " SQL-TEMP"))
	(original-buffer (current-buffer))
	(table-arg (sql-create-table-arg-for-bcp table)))
    (if sql-bcp-command-switches
	(call-process sql-bcp-command nil temp-buffer nil table-arg "in" file
		      (concat "-S" sql-server)
		      (concat "-U" sql-user)
		      (concat "-P" sql-password)
		      sql-bcp-command-switches)
      (call-process sql-bcp-command nil temp-buffer nil table-arg "in" file
		    (concat "-S" sql-server)
		    (concat "-U" sql-user)
		    (concat "-P" sql-password)
		    "-c" "-t|" "-r\n"))
    (set-buffer temp-buffer)
    (and (looking-at ".*DB-LIBRARY error") (ding))
    (sql-popup-dialog-return
     (list (buffer-substring (point-min) (point-max))
	   ["OK" 'no t]))
    (kill-buffer temp-buffer)
    (set-buffer original-buffer)))

(defun sql-bcp-in (table file)
  "Invoke bcp to copy TABLE into the current database from FILE.

Display any exit status in the echo area."
  (interactive (list (progn
		       (sql-goto-batch-buffer)
		       (setq sql-completion-saved-buffer (current-buffer))
		       (completing-read "Copy into table: "
					'sql-complete-table-list))
		     (expand-file-name (read-file-name "From file: "
						       nil nil t))))
  (let ((temp-buffer (get-buffer-create " SQL-TEMP"))
	(original-buffer (current-buffer))
	(table-arg (sql-create-table-arg-for-bcp table)))
    (if sql-bcp-command-switches
	(call-process sql-bcp-command nil temp-buffer nil table-arg "in" file
		      (concat "-S" sql-server)
		      (concat "-U" sql-user)
		      (concat "-P" sql-password)
		      sql-bcp-command-switches)
      (call-process sql-bcp-command nil temp-buffer nil table-arg "in" file
		    (concat "-S" sql-server)
		    (concat "-U" sql-user)
		    (concat "-P" sql-password)
		    "-c" "-t|" "-r\n"))
    (set-buffer temp-buffer)
    (goto-char (point-min))
    (replace-string "
" " ")
    (goto-char (point-min))
    (replace-string " Starting copy...  " "")
    (replace-string "   " " ")
    (and (looking-at ".*DB-LIBRARY error:") (ding))
    (message (buffer-substring (point-min) (point-max)))
    (kill-buffer temp-buffer)
    (set-buffer original-buffer)))

(defun sql-create-table-arg-for-bcp (table)
  (if (and (null sql-database) (null sql-bcp-user))
      table
    (concat sql-database "." sql-bcp-user "." table)))

(defun sql-edit-row ()
  "Edit the current row at point.  Simply make the change to the current
row by typing, and hit \\[sql-update] to perform the update.

While editing the current row, overwrite-mode is in effect to preserve
column alignment.  If the alignment is off, then the update will be
innacurate."
  (interactive)
  (or (eq major-mode 'sql-results-mode)
      (error "You must be in a sql-results-mode buffer."))
  (setq sql-edit-row-buffer (current-buffer))
  (add-hook 'post-command-hook 'sql-edit-row-sentinel)
  (sql-results-edit-mode)
  (message "Building column pairings...")
  (let* ((column-names (sql-build-column-names))
	 (column-widths (sql-build-column-widths)))
    (setq sql-update-virgin-column-pairs
	  (list (cons (current-line)
		      (sql-build-column-pairs column-names column-widths))))
    (setq sql-update-virgin-lines (list (current-line))))
  (message "Building column pairings... done")
  (message (substitute-command-keys
	    "Edit the buffer, then type \\[sql-update] to perform the update.")))

(defun sql-abort-update (msg)
  (message msg)
  (beep)
  (set-buffer sql-edit-row-buffer)
  (setq sql-update-virgin-column-pairs nil)
  (setq sql-update-virgin-lines nil)
  (sql-results-view-mode))

(defun sql-edit-row-sentinel ()
  (if (and (not (eq (current-buffer) sql-edit-row-buffer))
	   (not (window-minibuffer-p (selected-window))))
      (sql-abort-update "Aborting update row! You can't change buffers.")
    (if (not (member (current-line) sql-update-virgin-lines))
	(sql-add-line-to-update))))

(defun sql-add-line-to-update ()
  (message "Building column pairings...")
  (let* ((column-names (sql-build-column-names))
	 (column-widths (sql-build-column-widths)))
    (setq sql-update-virgin-column-pairs
	  (cons (cons (current-line)
		      (sql-build-column-pairs column-names column-widths))
		sql-update-virgin-column-pairs))
    (setq sql-update-virgin-lines
	  (cons (current-line) sql-update-virgin-lines)))
  (message "Building column pairings... done"))

(defun sql-update ()
  (interactive)
  "Update the rows that have changed in the results buffer.
This function should be invoked after \\[sql-edit-row] and after the rows
have been edited.

This will create a SQL statement which will have the effect of updating
the current database to reflect the manual editing that was performed."
  (or sql-update-virgin-column-pairs
      (error "Not ready for an update.  First you must edit the row."))
;  (or (eq last-command 'sql-edit-row)
;      (error "
;  (or sql-results-edit-mode
;      (error "Not ready for an update.  You must be in sql-results-edit minor mode."))
;  (or (eq (current-line) sql-update-virgin-line)
;      (progn
;	(message "%d " (current-line) sql-update-virgin-line) (sit-for 2)
;	(setq sql-update-virgin-column-pairs nil)
;	(setq sql-update-virgin-line nil)
;	(sql-results-view-mode)
;	(error "You are on a different line.  Please restart the update.")))
  (sql-goto-batch-buffer)
  (setq sql-completion-saved-buffer (current-buffer))
  (let ((default-table (sql-get-table-name)))
    (sql-goto-results-buffer)
    (let ((table (completing-read (format "Update in table (default %s): "
					  default-table)
				  'sql-complete-table-list)))
      (and (string-equal table "") (setq table default-table))
      (and (string-equal table "")
	   (sql-abort-update "Invalid table selection."))
      (sql-goto-batch-buffer)
      (or (assoc table sql-column-list)
	  (sql-get-columns table))
      (erase-buffer)
      (sql-goto-results-buffer)
      (let* ((column-names (sql-build-column-names))
	     (column-widths (sql-build-column-widths)))
	(setq sql-update-virgin-column-pairs
	      (reverse sql-update-virgin-column-pairs))
	(setq sql-update-virgin-lines
	      (reverse sql-update-virgin-lines))
	(while sql-update-virgin-column-pairs
	  (sql-goto-results-buffer)
	  (goto-line (car sql-update-virgin-lines))
	  (message "Building update statement for line %d..." (current-line))
	  (let ((column-pairs (sql-build-column-pairs column-names
						      column-widths))
		(virgin-pairs (cdr (assoc (current-line)
					  sql-update-virgin-column-pairs))))
	    (if (equal column-pairs virgin-pairs)
		(message "Nothing to update on line %d" (current-line))
	      (sql-goto-batch-buffer)
	      (goto-char (point-max))
	      (let ((differences (sql-remove-common column-pairs virgin-pairs))
		  (column-list (cdr (assoc table sql-column-list))))
		(insert "update " table "\nset ")
		(while differences
		  (let* ((column (car (car differences)))
			 (value (cdr (car differences)))
			 (no-quotes (or (string-equal "NULL" value)
					(string-match "\(.*\)" value)
					(string-match "\"\\|'" value)
					(string-equal "null" value)
					(eq 0 (cdr (assoc column
							  column-list))))))
		    (or (eq 2 (cdr (assoc column column-list)))
			(insert column " = "
				(if no-quotes "" "\"")
				value
				(if no-quotes "" "\"")
				",\n    "))
		    (setq differences (cdr differences))))
		(backward-delete-char 6)
		(insert "\nwhere ")
		(while virgin-pairs
		  (let* ((column (car (car virgin-pairs)))
			 (value (cdr (car virgin-pairs)))
			 (no-quotes (or (string-equal "NULL" value)
					(string-match "\(.*\)" value)
					(string-match "\"\\|'" value)
					(string-equal "null" value)
					(eq 0 (cdr (assoc column
							  column-list))))))
		    (or (eq 2 (cdr (assoc column column-list)))
			(insert column " = "
				(if no-quotes "" "\"")
				value
				(if no-quotes "" "\"")
				"\n  and "))
		    (setq virgin-pairs (cdr virgin-pairs))))
		(backward-delete-char 6)
		(insert "\n"))))
	  (setq sql-update-virgin-column-pairs
		(cdr sql-update-virgin-column-pairs))
	  (setq sql-update-virgin-lines
		(cdr sql-update-virgin-lines))))
      (goto-char (point-min))
      (sql-goto-results-buffer)
      (sql-results-view-mode)
      (sql-goto-batch-buffer))
    (message (substitute-command-keys
	      "Type \\[sql-evaluate-buffer] to perform the update."))))

(defun sql-remove-common (list1 list2)
  "Remove all elements in list1 that appear in list2."
  (let ((condensed-list nil))
    (while list1
      (or (member (car list1) list2)
	  (setq condensed-list (cons (car list1) condensed-list)))
      (setq list1 (cdr list1)))
    (reverse condensed-list)))

(defun current-line ()
  "Return the current line as an integer."
  (interactive)
  (+ (count-lines (point-min) (point))
     (if (eq (current-column) 0)
	 1
       0)))

(defun goto-column (column)
  "Goto column COLUMN on the current line."
  (interactive "nColumn: ")
  (beginning-of-line)
  (forward-char column))

(defun sql-build-column-widths ()
  (if sql-column-separator
      ()
    (save-excursion
      (or (search-backward "---" nil t)
	  (error "Can't find header line."))
      (beginning-of-line)
      (search-forward " ")
      (let ((column-widths nil)
	    (eol (save-excursion (end-of-line) (point))))
	(while (< (point) eol)
	  (let* ((column-start (current-column))
		 (column-end (progn (search-forward " ") (current-column))))
	    (setq column-widths (cons (- column-end column-start)
				      column-widths))))
	(reverse column-widths)))))

(defun sql-build-column-names ()
  (save-excursion
    (or (search-backward "---" nil t)
	(error "Can't find header line."))
    (forward-line -1)
    (skip-chars-forward " ")
    (let ((column-names nil)
	  (eol (save-excursion (end-of-line) (point))))
      (while (< (point) eol)
	(let* ((column-name (current-word)))
	  (setq column-names (cons column-name column-names))
	  (forward-word 1)
	  (skip-chars-forward " \t")))
      (reverse column-names))))

(defun sql-cut-data ()
  "Cut the row at point or the rows withing the region.
If the region is not active, cut the row at point.  Cut the rows within the
region if it is."
  (interactive)
  (or (eq major-mode 'sql-results-mode)
      (error "You must be in a sql-results-mode buffer."))
  (if (mark)
      (call-interactively 'sql-cut-region)
    (call-interactively 'sql-cut-row)))

(defun sql-copy-data ()
"Copy the row at point or the rows withing the region.
If the region is not active, copy the row at point.  Copy the rows within the
region if it is."
  (interactive)
  (or (eq major-mode 'sql-results-mode)
      (error "You must be in a sql-results-mode buffer."))
  (if (mark)
      (call-interactively 'sql-copy-region)
    (call-interactively 'sql-copy-row)))

(defun sql-paste-data ()
  "Paste the contents of the SQL Mode clipboard into the results buffer.
The data is pasted in the form of an insert statement, which can be reviewed
before execution."
  (interactive)
  (sql-goto-batch-buffer)
  (call-interactively 'sql-paste))
  
(defun sql-delete-data ()
  "Delete the row at point or the rows withing the region.
If the region is not active, delete the row at point.  Delete the rows within
the region if it is."
  (interactive)
  (or (eq major-mode 'sql-results-mode)
      (error "You must be in a sql-results-mode buffer."))
  (if (mark)
      (call-interactively 'sql-delete-region)
    (call-interactively 'sql-delete-row)))

(defun sql-iconify-frame ()
  (interactive)
  (iconify-frame))

(defun sql-build-column-pairs (names widths)
  (or sql-column-separator
      (eq (length names) (length widths))
      (error "Error building column pairs."))
  (if (not sql-column-separator)
      (sql-guess-column-pairs names widths)
    ))

(defun sql-guess-column-pairs (names widths)
  (save-excursion
    (beginning-of-line)
    (forward-char 1)
    (let ((column-pairs nil))
      (while names
	(let* ((name (car names))
	       (start (point))
	       (word-start (point))
	       (width (car widths))
	       (offset (if (and (< width 5) (string-equal (current-word)
							  "NULL"))
			   (- 5 width)
			 0))
	       (end (+ start width offset))
	       (value nil))
	  (skip-chars-forward " \t")
	  (setq word-start (point))
	  (goto-char end)
	  (skip-chars-backward " \t")
	  (setq end (point))
	  (setq value (if (> end word-start)
			  (buffer-substring word-start end)
			""))
	  (setq names (cdr names))
	  (setq widths (cdr widths))
	  (goto-char start)
	  (forward-char (+ width offset))
	  (setq column-pairs (cons (cons name value) column-pairs))))
      (reverse column-pairs))))

(defun sql-insert-a-space ()
  (interactive)
  (delete-char 1)
  (insert " ")
  (forward-char -1))

(defun sql-backward-insert-a-space ()
  (interactive)
  (forward-char -1)
  (delete-char 1)
  (insert " ")
  (forward-char -1))

(defun sql-magic-go ()
  (interactive)
  (insert "o")
  (and sql-use-magic-go
       (save-excursion (beginning-of-line) (looking-at "go$"))
       (cond ((eq major-mode 'sql-mode)
	      (and (buffer-live-p sql-evaluation-buffer)
		   (sql-mode-evaluate-statement)))
	     ((eq major-mode 'sql-batch-mode)
	      (sql-evaluate-buffer nil)))))

(defun sql-set-frame-icon (frame)
  "Set the icon of the current frame if in a sql related mode.
Suitable for use as a function for the hook unmap-frame-hook."
  (if (not (or (eq major-mode 'sql-mode)
	       (eq major-mode 'sql-batch-mode)
	       (eq major-mode 'sql-results-mode)
	       (eq major-mode 'sql-interactive-mode)))
      ()
    (require 'sql-icons)
    (save-excursion
      (if (framep frame)
	  (select-frame frame))
      (x-set-frame-icon-pixmap frame sql-frame-icon))))

(defun sql-set-window-configuration ()
  "NOT YET IMPLEMENTED

Set the various window configuration variables based on the current state.
The frame size, whether the results buffer is in a new frame, the results
buffer size, and various other attributes are consulted.

The appropriate variables are set, and then the user is prompted to save the
current option settings."
;  batch-in-new-frame
;  batch-height
;  batch-width
;  i-in-new-frame
;  i-height
;  i-width
;  r-in-new-frame
;  r-height
;  r-width
  (error "This function has not been implemented yet."))

(defun sql-show-news ()
  "Display the NEWS file that came with the distribution."
  (interactive)
  (let ((path load-path)
	(found nil))
    (while (and path (not found))
      (if (file-readable-p (concat (car path) "NEWS"))
	  (setq found t)
	(setq path (cdr path))))
    (if found
	(find-file (concat (car path) "NEWS"))
      (error "Could not find the NEWS file."))))

(defun sql-display-new-users-guide ()
  (and sql-noisy (ding))
  (let ((prompt "

  SQL Mode Version 0.922 (beta)

  You do not have an association list defined in your $HOME/.sql-mode
  file.  Having such associations defined makes interaction with SQL
  Mode much easier.  To find out how to set this up, you may refer to
  the comments at the top of the sql-mode.el file, or you may invoke
  the SQL Mode help system via C-c h (in a SQL Mode buffer).

  In the absence of associations, the only way to enter sql-batch-mode
  and sql-interactive mode is to manually enter the server, user and
  password on each invocation.  Type M-x sql-batch-mode to try it out.        

"))
    (and (if sql-xemacs
	     (sql-popup-dialog-return
	      (list prompt
		    ["OK\n Warn me next time" 'no t]
		    nil
		    ["OK\n Don't warn me again" t t]))
	   (x-popup-dialog t (cons prompt
				   '(("OK\n Warn me next time" . nil)
				     ("OK\n Don't warn me again" . t)))))
	 (sql-no-warn))))

(defun sql-no-warn ()
  "Make it so that we don't warn the user about not having an association list.
This file appends to the end of the user's $HOME/.sql-mode file."
  (interactive)
  (write-region "\n(setq sql-dont-warn t)\n" nil
		(concat (getenv "HOME") "/.sql-mode") t 1))

(defun sql-display-load-path-message ()
  "Display a message explaining how to add an item to the load-path variable."
  (interactive)
  (and sql-noisy (ding))
  (let ((prompt "

  SQL Mode Version 0.922 (beta)

  SQL Mode cannot find some files that it needs to function to it's fullest
  capacity.  The files that it cannot find are lisp files (with the .el
  extension) which contain many important function definitions that are
  essential for some of SQL Mode's more advanced features, like toolbars,
  icons, completion, and indentation.  Emacs uses the variable ``load-path''
  to search for lisp files, and currently this variable does not contain
  the directory in which you have installed SQL Mode.

  In order for SQL Mode to function properly, you need to alter the
  value of the variable load-path to contain the directory that SQL Mode
  currently resides.  Alternatively, you can move the SQL Mode lisp files
  into a directory that is in your load path.

"))
    (and (if sql-xemacs
	     (sql-popup-dialog-return
	      (list prompt
		    ["OK\n I'll take care\n of it myself" 'no t]
		    nil
		    ["OK\n Please alter the\n load-path variable for me" t t]))
	   (x-popup-dialog t (cons prompt
				   '(("OK\n I'll take care of it" . nil)
				     ("OK\n Please alter the\n load-path variable for me" . t)))))
	 (sql-prompt-for-load-path))))
  
(defun sql-prompt-for-load-path ()
  "Add lines to the users .emacs file to alter the load path.
Argument PATH is the path to append to the load path."
  (let ((prompt "

  In order to alter the load-path variable appropriately, you need to tell
  me where you have installed SQL Mode.  After clicking OK, please type in
  the name of the directory where I can find the SQL Mode lisp files.

"))
    (and (if sql-xemacs
	     (sql-popup-dialog-return
	      (list prompt
		    ["OK\n Get on with it already!" t t]
		    nil
		    ["CANCEL\n I changed my mind" 'no t]))
	   (x-popup-dialog t (cons prompt
				   '(("OK\n Get on with it already!" . t)
				     ("CANCEL\n I changed my mind" . nil)))))
	 (call-interactively 'sql-add-to-load-path))))

(defun sql-add-to-load-path ()
  "Add lines to the users .emacs file to alter the load path.
Argument PATH is the path to append to the load path."
  (interactive)
  (let* ((d (read-directory-name "SQL Mode directory: "))
	 (dir (expand-file-name d)))
    (if (not (nth 0 (file-attributes dir)))
	(error "Cannot find directory %s." dir)
      (set-buffer (find-file-noselect (expand-file-name "~/.emacs")))
      (insert (format "(setq load-path (cons \"%s\" load-path))\n" dir))
      (save-buffer)
;;      (write-region (format "\n(setq load-path (cons \"%s\" load-path))" dir)
;;		    nil (concat (getenv "HOME") "/.emacs") t 1)
      (setq load-path (cons dir load-path)))))

(defun sql-initialize ()
  "Initialize SQL related modes."
  (interactive)
  (sql-load-top-ten)
  (sql-load-customizations)
  (and sql-sybase (setenv "SYBASE" sql-sybase))
  (and sql-use-interfaces-file
       (eq sql-dataserver-type 'sybase)
       (sql-read-interfaces-file))
  (sql-setup-font-lock)
  (sql-add-minor-modes)
  (sql-add-menus)
  (or sql-association-alist
      sql-dont-warn
      (sql-display-new-users-guide))
;  (and sql-change-frame-icon
;       (add-hook 'unmap-frame-hook 'sql-set-frame-icon))
  (setq sql-initialized t)
  (condition-case nil
      (require 'sql-test)
    (error (sql-display-load-path-message)))
  (run-hooks 'sql-init-hook))

(autoload 'sql-toolbar "sql-toolbar.el"
  "Use a toolbar in SQL Mode" t)
(autoload 'sql-turn-on-toolbar "sql-toolbar.el"
  "Use a toolbar in SQL Mode" t)
(autoload 'sql-top-ten-toolbar "sql-toolbar.el"
  "Use a top ten toolbar in SQL Mode" t)
(autoload 'sql-turn-on-top-ten-toolbar "sql-toolbar.el"
  "Use a top ten toolbar in SQL Mode" t)
(autoload 'sql-indent-line "sql-indent.el"
  "Indent line as SQL code." t)
(autoload 'sql-show-syntactic-information "sql-indent.el"
  "Show the syntactic information at point." t)
(autoload 'sql-comment-line-toggle "sql-comment.el"
  "Comment out a line of SQL code, or un-comment it if it is commented." t)
(autoload 'sql-comment-buffer "sql-comment.el"
  "Comment out each line of SQL code in the current buffer." t)
(autoload 'sql-uncomment-buffer "sql-comment.el"
  "Remove comment from each line of SQL code in the current buffer." t)
(autoload 'sql-comment-buffer-toggle "sql-comment.el"
  "Toggle the comment state of each line in the current buffer." t)
(autoload 'sql-comment-region "sql-comment.el"
  "Comment out each line of SQL code in the current region." t)
(autoload 'sql-uncomment-region "sql-comment.el"
  "Uncomment each line of SQL code in the current region." t)
(autoload 'sql-comment-region-toggle "sql-comment.el"
  "Toggle comment state of each line in the current region." t)
    
(provide 'sql-mode)

(run-hooks 'sql-load-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sql-mode.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; planner.el --- The Emacs Planner

;; Copyright (C) 2001 John Wiegley <johnw@gnu.org>

;; Emacs Lisp Archive Entry
;; Filename: planner.el
;; Version: 1.5
;; Date: 2001-04-17
;; Keywords: hypermedia
;; Author: John Wiegley <johnw@gnu.org>
;; Maintainer: John Wiegley <johnw@gnu.org>
;; Description: Use Emacs for life planning
;; URL: http://www.gci-net.com/~johnw/Emacs/planner.el
;; Compatibility: Emacs20, Emacs21

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; Note:

;; This package extends emacs-wiki.el to act as a day planner, roughly
;; equivalent to the one used by Franklin-Covey.  If they have patents
;; and trademarks and copyrights to prevent me even thinking in terms
;; of their methodology, then I can't believe they care at all about
;; productivity.

;;; Usage summary:

;; * Make a planning file

;; Open a wiki file within your planning directory.  You will likely
;; have to do this using C-x C-f.

;; * Break it into stages

;; Start the file with your "vision", or the goal you intend to
;; accomplish.  Break this up into parts, and create a Wiki file for
;; each part, with defined milestones which constitute the "goal" for
;; that part.

;; * Write out the tasks for each stage

;; In each sub-plan, list out the tasks necessary to accomplish the
;; milestone.  Write them into the file like this:

;; A _ 1h Call so and so to make a reservation

;; * Decide on a priority for each task

;; The A is the priority of the task.  The _ means it isn't done yet,
;; and the 1h is a quick estimate on how long it will task.  The time
;; estimates are optional.

;; The priorities break down like this:
;;
;;  A: if you don't do it, your plan will be compromised, and you
;;     will have to either abort, backtrack, or make profuse apologies
;;     to someone
;;
;;  B: if you don't do it, your plan will be delayed
;;
;;  C: the plan won't be complete until it's done, but there's no
;;     pressure to do it now

;; * Schedule the tasks

;; Put your cursor on a line containing a task, and type C-c C-c.
;; This will copy the task to a specific day, which you will be
;; prompted for.  The Emacs Calendar pops up, so you can pick a free
;; day (if you use the Emacs diary and appointment system, the
;; Calendar is even more useful).

;; You will now see your new task, with a link back to your planning
;; page.  Selecting this link will take you back to that task on the
;; planning page, where you will see that the planning page's task now
;; has a link to the particular day you scheduled the task for.

;; The two tasks (the one on the planning page, and the one on the
;; daily task list) are linked.  Changing the status of one (using C-c
;; C-x, or C-c C-s, for example) will change the status of the other.
;; If you forward the task to another day (using C-c C-c on the daily
;; task page), the planning page's link will be updated to refer to
;; the new day.  This is so that you can focus on your daily task list
;; during the day, but see an overview of your plan's progress at any
;; time.

;; * Do the work

;; That's it, as far as what planner.el can do.  Planning is an art,
;; just as estimating time is an art.  It happens with practice, and
;; by thinking about these things.  The Commentary below provides a
;; few of my own thoughts on the matter, although I will say that this
;; an art I have yet to truly develop.

;;; Commentary:

;; What is planning?  It can be a nebulous thing to define.  In its
;; essence, however, it is very simple: it's how we achieve our dreams.

;; Our days are filled with time, and hence with actions, whether they
;; be of a mental or physical sort.  But there are two kinds of
;; action: reactive and creative.  Reactive action is a response to
;; the environment, a reaction to stimulus.  Had we enough instincts
;; to ensure survival, we could live according to this kind of action
;; alone.  It is a mode of behavior we share with every living
;; species.

;; The opposite to reactivity is creativity, when we decide upon a
;; course of action that is a wholly a product of personal choice.  We
;; then make decisions as to the steps needed to make this wish a
;; reality.  This is planning.  Planning is essentially a creative
;; endeavor at every step.

;; First, create the idea, what you want to achieve.  Very short-term
;; ideas do not need much more than thinking about how to do it.  But
;; long-term ideas require planning, since the mind cannot contain all
;; of the details.

;; Second, decide how the idea maps into the circumstances you find
;; yourself in.  Some environments will assist your plan, others
;; hinder it.  But step by step, identify every barrier to the
;; realization of your idea, and devise a countermeasure to overcome
;; it.  Once you've mapped things out from beginning to end,
;; accounting for unknowables as best you can, you now have your plan.

;; Third is to break the stages of the plan into parts that are not
;; overwhelming in their complexity.  It is at during this phase that
;; a plan is turned into task items, each to be accomplished within
;; the span of one day's time.  If a task requires several days, break
;; it up further.  The smaller it is, the less your mind will recoil
;; from attempting it.

;; Fourth is to monitor your progress, identifying problems and
;; correcting for them as you go.  Some plans start out unachievable,
;; and remain that way indefinitely, due to a simple lack of
;; observation.  If nothing is working for you, change it.  Otherwise,
;; your plan is merely a well-crafted wish.

;; Fifth is just to do the work, and be patient.  All good plans take
;; a great deal of time, and *cannot* happen immediately.  The
;; groundwork must be laid for each step, or else it will rest on an
;; unsecure foundation.  If you follow your plan doggedly, applying
;; some time to it each day or week, it _will_ happen.  Remember the
;; story of the tortoise and the hare.  I've even written a short
;; essay on the necessity of gradual accomplishment, which can be
;; found at http://www.gci-net.com/~johnw/AdvancingTheProcess.html.

;; How can this software help?  Computers are ideal for manipulating
;; information, since they allow you to change things without erasing
;; or rewriting.  And since all plans change quite a bit during their
;; implementation, a planning program can be very helpful.

;; Start by adding the following to your .emacs file:

;;   (load "planner")

;; Now, conceive your idea.  I can't believe there's nothing you want
;; from life.  More peace, time to enjoy the world, an end to war?
;; Everyone wants something.  Search deeply, and you will find
;; countless unhoped wishes lurking therein.  Choose one for now, and
;; think on it for a while.

;; Then type M-x emacs-wiki-find-file.  You will probably want to bind
;; this to a key in your .emacs file:

;;   (define-key mode-specific-map [?w] 'emacs-wiki-find-file)

;; So, do that and type C-c w.  It will ask you for the name of a Wiki
;; page.  I'm going to leave the discussion of Wikis and what they are
;; to emacs-wiki.el, since it doesn't really matter here.

;; Type in a name for your plan, such as "BetterHealth".  However, you
;; must choose an idea you really want to accomplish.  Struggle to
;; differentiate between the things you want because others want them,
;; and the things you want for yourself.  It takes quite an effort,
;; and may require a long time before you notice the difference.  Many
;; people want to be more healthy to be more attractive, which is an
;; externally driven goal.  Unless _you_ really want to accomplish
;; what you envision, the odds are you will fail.  Only our own wishes
;; and dreams possess enough personal energy to see themselves to
;; fruition.  What happens to many of us is simply that we never
;; become conscious of these dreams: what we love, what we desire
;; most.  When I talk to friends, so much of what I hear is things
;; they want because they feel they should want them.  There's just
;; not enough energy there to pursue a good plan, because nearly all
;; of it is negative energy.

;; Do you know what you really want?  Don't worry, many people don't.
;; It's not a question anyone really wants us to pursue, because often
;; we don't want what others do; it doesn't contribute to the social
;; welfare, and all that nonsense.  Somehow we always forget that
;; what's good for the social welfare now, was someone else's crazy
;; dream a hundred years ago.  The human aversion to fundamental
;; change is always one's greatest enemy, so don't waste any time
;; getting bitter about it.

;; For the sake of argument I assume you really do want to be
;; healthier, because you've fallen in love with the ideal of purity,
;; or you understand the connection between your physical self and the
;; world around you, and how this can open up your spirit to desiring
;; more.  I assume.  :)

;;   M-x emacs-wiki-find-file RETURN BetterHealth RETURN

;; You're now in a Wiki file called BetterHealth.  Start typing.  Type
;; anything related to your idea: what you think about it, your ideas
;; on it, *and especially what the end will look like*.  If you can't
;; visualize the end, you can't plan, since planning is about drawing
;; a line between now and then.

;; When you've typed enough to gain a vision of your goal, start
;; drafting what the possible intermediate steps might be.  Then stop,
;; get up, walk around, enjoy life, and come back to it.  Taking a
;; long time at the beginning is not a bad idea at all, as long as
;; it's not forever.

;; As you chew on your idea, it will begin to become more and more
;; concrete.  You'll have ideas about the smallest pieces, and ideas
;; about the biggest pieces.  Keep going until it starts to take shape
;; before you, and you can see yourself in your mind's eye moving from
;; the present into the future.  Write down this progression, and the
;; sorts of things you might encounter along the way.

;; As you continue, you'll naturally discover discrete phases, or
;; "milestones" as managers love to call them.  These are very
;; important, because they let you know you're making progress.  I
;; recommend having a big party with friends every time you achieve a
;; milestone.  A typical plan might have between three and ten.

;; Between the milestones are the bigger pieces of your plan.  Name
;; these pieces using MixedCase words, and you'll notice that Emacs
;; colors and underlines them for you.  Like, FindGoodGym.  Hit return
;; on this highlighted word, and you'll find yourself in another,
;; blank file.  In this file, start drafting your sub-plan, just as
;; you did with the larger plan.  You should find it easier now, since
;; the scope is smaller.

;; As you break down further, you'll notice simple little things that
;; need to get done.  These are your tasks.  Every plan is a
;; succession of tasks.  The difference from reactivity is that each
;; task is part of the larger plan.  This is what it means to be
;; systematic: that everything you do helps further your plan.  If you
;; have tasks in your day that contribute to no plan, they are
;; reactive.  Of course, life is full of these, but don't let them
;; take up more than 20% of your day.  If you allow yourself to be
;; dominated by reactive tasks, you'll regret it at the end of your
;; life.  I don't know this personally, but I do know that striving
;; for one's dreams -- and seeing them come to fruition -- is the
;; greatest joy a man can possess.  It is the essence of freedom, of
;; living, of creation.  Reactivity is the opposite of this, and
;; serves only to drain our energy and slacken our spirits.

;; Now that you've identified a simple task, type C-c C-t.  This will
;; ask for a brief description of the task, and when you plan to do
;; it.  If you hit RETURN at the question 'When', it assumes you mean
;; today.  The Planner will also pop up a three-month calendar at this
;; question, so you can see where your free days are.  Make sure you
;; set the variable `mark-diary-entries-in-calendar' to t in your
;; .emacs file.  This way, you can see which days your appointments
;; fall on.  (Read about the Emacs Calendar and Diary in the Emacs
;; info manual).

;;   (setq mark-diary-entries-in-calendar t)

;; Once your task is in there, go back to your plan and keep
;; generating more tasks.  Generate them all!  Fully describe -- as
;; tasks -- everything necessary to bring your sub-plan to completion.
;; Don't create tasks for the other sub-plans.  You may have good idea
;; of what they'll look like, but don't bother rendering them into
;; tasks just yet.  Things will change too much between now and then,
;; for that to be a good use of your time.

;; Is your sub-plan now rendered into all of the tasks necessary to
;; reach your first milestone?  Great!  That is the purpose of
;; planner.el.  The rest is really up to you.  If you find that you
;; keep putting things off, and never do them, that's the surest sign
;; you're planning for someone else's dream, and not your own.

;; Here are some of the things planner.el can do, to help you manage
;; and track your tasks:

;; At the beginning of every day, type M-x plan.  This will jump you
;; to the top of the most recent task list before today.  If you
;; skipped a bunch of days, you'll have to open up those files on your
;; own.

;; Probably some of the tasks that day won't be finished -- that's OK.
;; Learning to properly estimate time is a magical, mystical art that
;; few have mastered.  Put your cursor on those undone tasks, and type
;; C-c C-c.  This will move them into today's task page.  You can jump
;; to today's task page at any time by typing C-c C-n (from a Wiki or
;; planning page).  I heartily recommend binding C-c n, to jump you to
;; this page from anywhere:

;;   (define-key mode-specific-map [?n] 'planner-goto-today)

;; As you look at your task sheet each day, the first thing to do is
;; to "clock in" to one of them.  This isn't necessary, and is only
;; helpful if you're around your computer a lot.  But by typing C-c
;; C-i (assuming you have my timeclock.el on your load-path), it will
;; log the time you spend working on your sub-plan.  This is helpful
;; for viewing your progress.  Type C-c C-o to clock out.

;; C-c C-u and C-c C-d will move a task up and down in priority.  The
;; priority scheme has two components: a letter A through C, and a
;; number from 1 onwards.  'A' tasks mean they must be done that day,
;; or else your plan is compromised and you will have to replan.  'B'
;; means they should be done that day, to further the plan, otherwise
;; things will be delayed.  'C' means you can put off the task if you
;; need to, although ultimately it will have to be done.

;; For reactive tasks, the letters mean something different: 'A' means
;; you must do it today, or somebody will roast your chestnuts over an
;; open fire.  'B' means you should do it today, or else someone will
;; be practicing patience at the day's end.  'C' means no one will
;; notice if you don't do it.

;; Again, reactive tasks are ENEMIES OF PLANNING.  Really, until you
;; see them that way, circumstances will push you around and steal
;; your life away.  We have only so many years to use, and everyone is
;; greedy to take them.  It's insidious, almost invisible.  A healthy
;; dislike of reactivity will do wonders for organizing your affairs
;; according to their true priority.

;; The last word that needs to be said concerns "roles".  Every person
;; stands in several positions in his life: husband, employee,
;; manager, etc.  These roles will tend to generate tasks not
;; associated with any immediate plan, but necessary to maintain the
;; health and functioning of the role.  My suggestion is to keep this
;; the smallest possible number, and fulfill those that remain well.
;; How you decide to apportion your time between pursuing grand
;; designs, and fostering deep relationships, is a personal matter.
;; If you choose well, each will feed the other.

;; I mention this to point that reactivity is something not
;; exclusively associated with tasks that have no master plan, because
;; being a father, for example, is something that rarely proceeds
;; according to orderly plans.  But the role of father itself is its
;; own plan, whose goal is "to be the best one can", and whose
;; component tasks are spending time on whatever comes up.  It is, in
;; a sense, an implicit plan.  But reactive tasks follow no plan at
;; all; they are parasites of time that suck the spirit away, whereas
;; properly chose roles actually help fulfill one's own inner needs.
;; At least, this is what I believe.  And now back to technical
;; matters:

;; C-c C-l can be used at any time to refresh and renumber all of your
;; tasks, according to their actual order in the buffer.  You don't
;; need to use C-c C-u and C-c C-d (mentioned above).  Just edit them
;; normally and type C-c C-l.

;; Here is a summary of the keystrokes available, including a few I
;; did not mention:
;;
;;   M-x plan  Begin your planning session.  This goes to the last
;;             day for which there is any planning info (or today if
;;             none), allowing you to review, and create/move tasks
;;             from that day.
;;
;;   C-c C-u   Raise a task's priority
;;   C-c C-d   Lower a task's priority
;;   C-c C-l   Refresh, and renumber all tasks
;;
;;   C-c C-s   Mark the task as in progress or delegated
;;   C-c C-x   Mark the task as finished
;;
;;   C-c C-t   Create a task associated with the current Wiki page
;;             If you are on the opening line of a Note entry, it is
;;             assume that the note itself is the origin of the task.
;;   C-c C-c   Move or copy the current task to another date
;;             If the current task is an original (meaning you are in
;;             the buffer where's defined, hopefully a planning page)
;;             then it will be copied, and the original task will also
;;             now point to the copy.  If the current task is a copy,
;;             it will just be moved to the new day, and the original
;;             tasks link will be updated.
;;
;;   C-c C-n   Jump to today's task page
;;
;; Also, in the Emacs Calendar, typing 'n' will jump to today's task
;; page.

;; The format of a planning file is given below.  You are responsible
;; for keeping it looking like this.  I intentionally did not make
;; planner.el heavy on the UI side of things, too keep it more
;; free-form and open.  This lets you adapt it to whatever your
;; particular preferences might be.

;;----------------------------------------------------------------------
;; * Tasks
;;
;; #A1 _ An open task, very important!
;; #A2 X A closed task (MyPlan)
;; #A3 o A task that's delayed, or delegated (MyPlan)
;; #B1 _ Open task again, reacting to [[bbdb://John Wiegley]]
;; #B2 _ Hey, this task came from [[gnus://nnml:mail.personal/100]]
;;
;; Use `A n' in the Gnus summary buffer to create tasks based on an
;; e-mail request, complete with the correct cross-reference.  Hitting
;; enter on the above [[gnus link]] will bring up that e-mail.  E-mail
;; generated tasks are nearly also reactive, though.  Watch out!
;;
;; Clicking on a `bbdb' URL will perform a search for that name/text
;; in your BBDB.
;;
;; * Notes
;;
;; .#1 This is note number one
;;
;; Notes on note number one!  Here's a task reference to a task within
;; this file: (#A1).  An old task reference is done with
;; (2000.10.20#A2) or (10.20#A3) or even (20#A3).  Whatever you leave
;; out, it will assume it's the same as the current file's year/month.
;; You can even reference other notes in this file or in other files:
;; (#2), (20#2), etc.
;;
;; .#2 This wierd ".#2" syntax is used because it's what allout.el
;;     likes for enumerated lists, and it makes using
;;     outline-minor-mode (with allout) very handy.  You can omit the
;;     leading period if you like, though.  It's optional.
;; ----------------------------------------------------------------------

;;; Code:

(require 'emacs-wiki)

;;; Options:

(defgroup planner nil
  "An extension of Emacs-Wiki for doing time planning in Emacs.")

(defcustom planner-mode-hook nil
  "A hook that is run when planner mode is entered."
  :type 'hook
  :group 'planner)

(defcustom planner-directory "~/Plans"
  "The directory that contains your planning files."
  :type 'directory
  :set (function
	(lambda (sym val)
	  ;; remove the previous planner directory
	  (if (boundp 'planner-directory)
	      (setq emacs-wiki-directories
		    (delq planner-directory emacs-wiki-directories)))
	  (unless (member val (emacs-wiki-directory-list))
	    (nconc emacs-wiki-directories (list val)))
	  (set sym val)))
  :group 'planner)

(defcustom planner-name-regexp
  (concat "\\([1-9][0-9][0-9][0-9]\\.\\)?\\([0-9][0-9]\\.\\)?"
	  "[0-9][0-9]#[A-C][1-9][0-9]*\\|"
	  "[0-9]\\{4\\}\\.[0-9]\\{2\\}\\.[0-9]\\{2\\}")
  "A regexp used to match planner references in a planning buffer."
  :type 'regexp
  :group 'planner)

(defcustom planner-url-regexp
  (concat "\\<\\(https?:/?/?\\|ftp:/?/?\\|gopher://\\|"
	  "telnet://\\|wais://\\|file:/\\|s?news:\\|"
	  "bbdb:\\|gnus:\\|mailto:\\)"
	  "[^]	\n \"'()<>[^`{}]*[^]	\n \"'()<>[^`{}.,;]+")
  "A regexp used to match URLs within a Wiki buffer."
  :type 'regexp
  :group 'planner)

(defcustom planner-use-bbdb t
  "If non-nil, use BBDB to determine people's real names."
  :type 'boolean
  :group 'planner)

(defcustom planner-marks-regexp "[_oX>]"
  "Regexp that matches status character for a task."
  :type 'regexp
  :group 'planner)

(defvar planner-mode-map
  (let ((map (copy-keymap emacs-wiki-mode-map)))
    (define-key map [(control ?c) (control ?n)] 'planner-goto-today)
    (define-key map [(control ?c) (control ?v)] 'planner-goto)

    (define-key map [(control ?c) (control ?i)] 'planner-clock-in)
    (define-key map [(control ?c) (control ?o)] 'timeclock-out)

    (define-key map [(control ?c) (control ?t)] 'planner-create-task)
    (define-key map [(control ?c) (control ?c)] 'planner-copy-or-move-task)
    (define-key map [(control ?c) (control ?u)] 'planner-raise-task)
    (define-key map [(control ?c) (control ?d)] 'planner-lower-task)

    (define-key map [(control ?c) (control ?z)] 'planner-task-in-progress)
    (define-key map [(control ?c) (control ?x)] 'planner-task-done)

    (define-key map [(control ?c) return]       'planner-show-end-project)
    (define-key map [(control ?c) (control ?m)] 'planner-show-end-project)
    map)
  "Keymap used by Planner mode.")

;; Code:

(defvar planner-date-regexp
  "\\`\\([1-9][0-9][0-9][0-9]\\)\\.\\([0-9]+\\)\\.\\([0-9]+\\)\\'")

(defun planner-current-task-info ()
  (save-excursion
    (beginning-of-line)
    (when (looking-at (concat "^#?\\([A-C]\\)\\([0-9]*\\) \\("
			      planner-marks-regexp "\\) \\(.+\\)"))
      (let ((category (match-string-no-properties 1))
	    (priority (match-string-no-properties 2))
	    (status (match-string-no-properties 3))
	    (description (match-string-no-properties 4))
	    link-text link)
	(if (= (length priority) 0)
	    (setq priority nil))
	(when (string-match
	       "\\s-+\\((\\([^#)]*\\)\\(#[^)]+\\)?)\\|\\[\\[.+\\]\\]\\)"
	       description)
	  (setq link-text (match-string 1 description)
		link (or (match-string 2 description)
			 (emacs-wiki-wiki-base link-text)))
	  (setq description (replace-match "" t t description)))
	(list (emacs-wiki-page-name)
	      category priority status description link link-text)))))

(defsubst planner-task-page (info)	  (nth 0 info))
(defsubst planner-task-category (info)	  (nth 1 info))
(defsubst planner-task-priority (info)	  (nth 2 info))
(defsubst planner-task-status (info)	  (nth 3 info))
(defsubst planner-task-description (info) (nth 4 info))
(defsubst planner-task-link (info)	  (nth 5 info))
(defsubst planner-task-link-text (info)   (nth 6 info))

(defsubst planner-task-estimate (info)
  (if (string-match "\\`\\s-*\\([0-9]+[smhdw]\\)"
		    (planner-task-description info))
      (schedule-duration-to-seconds
       (match-string 1 (planner-task-description info)))))

(defun planner-end-projection ()
  "Show when today's task load will be finished, according to estimates."
  (require 'schedule)
  (schedule-initialize)
  (save-excursion
    (let ((now (schedule-completion-time (current-time) 0))
	  spent remaining slippage finish)
      (goto-char (point-min))
      (while (re-search-forward "^#[A-C]" nil t)
	(let* ((task (planner-current-task-info))
	       (estimate (planner-task-estimate task)))
	  (if estimate
	      (setq now (schedule-completion-time now estimate)))))
      now)))

(defun planner-show-end-project ()
  (interactive)
  (message (format-time-string "%c" (planner-end-projection))))

;;;###autoload
(defun plan ()
  "Start your planning for the day, beginning with the last day's tasks."
  (interactive)
  (let ((names
	 (sort (mapcar
		(function
		 (lambda (pair)
		   (if (string-match planner-date-regexp (car pair))
		       (car pair))))
		(emacs-wiki-file-alist))
	       (function
		(lambda (l r)
		  (string-lessp r l)))))
	(today (format-time-string "%Y.%m.%d"))
	opened)
    (while names
      (if (and (car names)
	       (string-lessp (car names) today))
	  (let ((emacs-wiki-directories (cons planner-directory
					      emacs-wiki-directories)))
	    (emacs-wiki-find-file (car names))
	    (setq opened t names nil))
	(setq names (cdr names))))
    (unless opened
      (planner-goto-today)
      (planner-seek-to-first))))

(defun planner-jump-to-linked-task ()
  (interactive)
  (let ((task-info (planner-current-task-info)))
    (when (and task-info (planner-task-link task-info))
      (emacs-wiki-find-file (planner-task-link task-info))
      (goto-char (point-min))
      (if (search-forward (planner-task-description task-info) nil t)
	  (beginning-of-line))
      t)))

(defun planner-renumber-tasks ()
  (save-excursion
    (goto-char (point-min))
    (let ((counters (list (cons "A" 1) (cons "B" 1) (cons "C" 1))))
      (while (re-search-forward "^#\\([A-C]\\)\\([0-9]\\)" nil t)
	(let ((counter (assoc (match-string 1) counters)))
	  (replace-match (number-to-string (cdr counter)) t nil nil 2)
	  (setcdr counter (1+ (cdr counter))))))))

(defun planner-raise-task (&optional arg)
  (interactive)
  (beginning-of-line)
  (kill-line 1)
  (forward-line (or arg -1))
  (save-excursion (yank)))

(defun planner-lower-task ()
  (interactive)
  (planner-raise-task 1))

(defun planner-mark-task (mark &optional this-only)
  (save-excursion
    (beginning-of-line)
    (re-search-forward planner-marks-regexp)
    (replace-match mark)
    (unless this-only
      (save-window-excursion
	(if (planner-jump-to-linked-task)
	    (planner-mark-task mark t))))))

(defun planner-task-in-progress ()
  (interactive)
  (planner-mark-task "o"))

(defun planner-task-done ()
  (interactive)
  (planner-mark-task "X"))

(defun planner-task-delegated ()
  (interactive)
  (planner-mark-task ">"))

(defun planner-task-pending ()
  (interactive)
  (planner-mark-task "_"))

(defsubst planner-today ()
  (format-time-string "%Y.%m.%d" (current-time)))

(defun planner-clock-in ()
  (interactive)
  (let ((task-info (planner-current-task-info)))
    (timeclock-in nil (if (planner-task-link task-info)
			  (concat (planner-task-link task-info) ": "
				  (planner-task-description task-info))
			(planner-task-description task-info)))
    (planner-task-in-progress)))

;;;###autoload
(defun planner-calendar-goto ()
  "Goto the planning file corresponding to the calendar date."
  (interactive)
  (let ((cdate (calendar-cursor-to-date))
	(emacs-wiki-directories (cons planner-directory
				      emacs-wiki-directories)))
    (emacs-wiki-find-file
     (format "%04d.%02d.%02d" (nth 2 cdate) (nth 0 cdate) (nth 1 cdate))
     'find-file-other-window)))

;;;###autoload
(defun planner-goto-today ()
  "Jump to the planning page for today."
  (interactive)
  (let ((emacs-wiki-directories (cons planner-directory
				      emacs-wiki-directories)))
    (emacs-wiki-find-file (planner-today) 'find-file-other-window)))

;;;###autoload
(defun planner-goto (date)
  "Jump to the planning page for today."
  (interactive (list (planner-read-date)))
  (let ((emacs-wiki-directories (cons planner-directory
				      emacs-wiki-directories)))
    (emacs-wiki-find-file (planner-expand-name date)
			  'find-file-other-window)))

(defun planner-read-date ()
  "Prompt for a date string in the minibuffer."
  (save-window-excursion
    (progn
      (calendar)
      (read-string "When (ex: 2001.3.31, 3.31, 31): "))))

(defun planner-seek-to-first ()
  (goto-char (point-min))
  (if (eobp)
      (progn
	(insert "* Tasks\n\n")
	(save-excursion
	  (insert "\n* Notes\n")))
    (forward-line 2)))

(defvar planner-task-annotation nil
  "If set, use as the annotation for the current task.")

(defun planner-create-task (title date)
  "Create a new task based on the current Wiki page.
It's assumed that the current Wiki page is the page you're using to
plan an activity.  Any time accrued to this task will be applied to
that page's name in the timelog file, assuming you use timeclock."
  (interactive (list (read-string "Describe task: ")
		     (unless current-prefix-arg
		       (planner-read-date))))
  (let* ((origin (if (looking-at "^\\.#\\([0-9]+\\)")
		     (concat (emacs-wiki-page-name) "#" (match-string 1))
		   (emacs-wiki-page-name)))
	 (origin-buffer (and origin (current-buffer)))
	 (emacs-wiki-directories (cons planner-directory
				       emacs-wiki-directories)))
    (if current-prefix-arg
	(emacs-wiki-find-file "TaskPool")
      (if (and date (> (length date) 0))
	  (emacs-wiki-find-file (planner-expand-name date))
	(planner-goto-today)))
    (planner-seek-to-first)
    (insert "#A0 _ " title)
    (if planner-task-annotation
	(insert " " planner-task-annotation)
      (insert " (" origin ")"))
    (insert "\n")))

(defun planner-copy-or-move-task (&optional date)
  "Move the current task to another day.
If this is the original task, it copies it instead of moving.
Most of the time, the original should be kept in a planning file,
but this is not required.
It also works for creating tasks from a Note.
This function is the most complex aspect of planner.el."
  (interactive (list (planner-expand-name (planner-read-date))))
  (beginning-of-line)
  (let ((task-info (planner-current-task-info)))
    (unless task-info
      (error "There is no task on the current line"))
    (if (planner-task-link task-info)
	(save-excursion
	  (kill-line 1)
	  (goto-char (point-min))
	  (if (looking-at "\\`\\* Tasks[ \t\n]+\\* Notes[ \t\n]+\\'")
	      (let ((filename buffer-file-name))
		(set-buffer-modified-p nil)
		(kill-buffer (current-buffer))
		(delete-file filename)))
	  (emacs-wiki-find-file (planner-task-link task-info))
	  (goto-char (point-min))
	  (when (search-forward (planner-task-description task-info)
				nil t)
	    (beginning-of-line)
	    (if (search-forward
		 (concat "(" (planner-task-page task-info) ")")
		 (line-end-position) t)
		(replace-match (concat "(" date ")") t t))))
      (if (planner-task-priority task-info)
	  (planner-task-delegated))
      (end-of-line)
      (insert " (" date ")"))
    (emacs-wiki-find-file date)
    (planner-seek-to-first)
    (insert "#" (planner-task-category task-info) "0 _ "
	    (planner-task-description task-info) " ("
	    (or (planner-task-link task-info)
		(planner-task-page task-info)) ")\n")))

(defun planner-get-message-id ()
  "Return the message-id of the current message."
  (save-excursion
    (set-buffer (get-buffer gnus-article-buffer))
    (set-buffer gnus-original-article-buffer)
    (goto-char (point-min))
    (let ((case-fold-search t))
      (if (re-search-forward "^Message-ID:\\s-*\\(<.+>\\)" (point-max) t)
	  (match-string 1)))))

(defun planner-get-from ()
  "Return the address of the sender of the current message."
  (save-excursion
    (set-buffer (get-buffer gnus-article-buffer))
    (set-buffer gnus-original-article-buffer)
    (goto-char (point-min))
    (let ((case-fold-search t))
      (if (re-search-forward "^From:\\s-*\\(.+\\)" (point-max) t)
	  (let ((addr (mail-extract-address-components (match-string 1))))
	    (if planner-use-bbdb
		(let ((rec (apply 'bbdb-search-simple addr)))
		  (if rec
		      (bbdb-record-name rec)
		    (or (car addr) (cadr addr))))
	      (or (car addr) (cadr addr))))))))

(defun planner-task-from-gnus ()
  "Generate a new task, based on a mail message."
  (interactive)
  (let* ((newsgroup gnus-newsgroup-name)
	 (planner-task-annotation
	  (concat " [[gnus://" newsgroup "/" (planner-get-message-id)
		  "][(E-Mail from " (planner-get-from) ")]]")))
    (call-interactively 'planner-create-task)))

(defun planner-browse-url (url)
  (cond
   ((string-match "^gnus://\\(.+\\)/\\(.+\\)" url)
    (let ((group (match-string 1 url))
	  (article (match-string 2 url)))
      (unless (and (fboundp 'gnus-alive-p)
		   (gnus-alive-p))
	(gnus-unplugged))
      (switch-to-buffer "*Group*")
      (gnus-group-jump-to-group group)
      (gnus-group-select-group)
      (gnus-summary-goto-article article)))
   ((string-match "^bbdb://\\(.+\\)" url)
    (bbdb (match-string 1 url) nil))
   (t (browse-url url))))

(defun planner-expand-name (name)
  "Expand the given NAME to its fullest form.
This typically means that dates like 3.31 will become 2001.03.31."
  (let ((bufname (and buffer-file-name
		      (file-name-nondirectory buffer-file-name)))
	year month)
    (if (and bufname (string-match planner-date-regexp bufname))
	(setq year (string-to-number (match-string 1 bufname))
	      month (string-to-number (match-string 2 bufname)))
      (let ((now (decode-time (current-time))))
	(setq year (nth 5 now)
	      month (nth 4 now))))
    (setq year (format "%04d." year)
	  month (format "%02d." month))
    (if (= (length name) 0)
	(planner-today)
      (if (string-match (concat "\\([1-9][0-9][0-9][0-9]\\.\\)?"
				"\\(\\([0-9]+\\)\\.\\)?"
				"\\([0-9]+\\)\\(#.*\\)?") name)
	  (concat
	   (or (match-string 1 name) year)
	   (if (match-string 2 name)
	       (format "%02d."
		       (string-to-int (match-string 3 name)))
	     month)
	   (format "%02d" (string-to-int (match-string 4 name)))
	   (match-string 5 name))
	name))))

(defvar planner-mode nil)

;;;###autoload
(define-derived-mode planner-mode emacs-wiki-mode "Planner"
  "An extension to Emacs Wiki that supports a planning system."
  (setq planner-mode t)

  (make-local-hook 'write-file-hooks)
  (add-hook 'write-file-hooks 'planner-renumber-tasks nil t)

  (set (make-variable-buffer-local 'emacs-wiki-browse-url-function)
       'planner-browse-url)

  (make-variable-buffer-local 'emacs-wiki-name-regexp)
  (make-variable-buffer-local 'emacs-wiki-url-regexp)
  (make-variable-buffer-local 'emacs-wiki-url-or-name-regexp)
  (make-variable-buffer-local 'emacs-wiki-highlight-regexp)

  (setq emacs-wiki-url-regexp planner-url-regexp
	emacs-wiki-name-regexp
	(concat "\\(" emacs-wiki-name-regexp
		"\\|" planner-name-regexp "\\)")
	emacs-wiki-url-or-name-regexp
	(concat "\\(" emacs-wiki-name-regexp
		"\\|" emacs-wiki-url-regexp "\\)")
	emacs-wiki-highlight-regexp
	(concat "\\(?:" emacs-wiki-highlight-regexp
		"\\|" planner-name-regexp "\\)"))

  (add-to-list 'emacs-wiki-publishing-markup
	       ["^#\\([A-C]\\)" 0 "- **\\1** "])
  (add-to-list 'emacs-wiki-publishing-markup
	       ["^#\\([A-C][0-9]+\\)" 0 "- <a name=\"\\1\"/>**\\1** "])
  (add-to-list 'emacs-wiki-publishing-markup
	       ["^\\.#\\([0-9]\\)" 0 "** <a name=\"\\1\"/>\\1. "]))

(defun planner-maybe ()
  (if (equal (directory-file-name
	      (expand-file-name
	       (file-name-directory buffer-file-name)))
	     (expand-file-name planner-directory))
      (planner-mode)
    (emacs-wiki-maybe)))

(put 'planner-mode 'flyspell-mode-predicate 'emacs-wiki-mode-flyspell-verify)

(remove-hook 'find-file-hooks 'emacs-wiki-maybe)
(add-hook 'find-file-hooks 'planner-maybe)
(add-hook 'emacs-wiki-before-publish-hook 'planner-maybe)

(defun planner-set-calendar-goto ()
  (define-key calendar-mode-map "n" 'planner-calendar-goto))

(add-hook 'calendar-load-hook 'planner-set-calendar-goto)

(unless (featurep 'httpd)
  (eval-after-load "gnus-sum"
    '(define-key gnus-summary-article-map "n" 'planner-task-from-gnus)))

;;; Code to generate a report from timeclock

;; This can be inserted into WikiPage (for example, named
;; CurrentStatus) like this:
;;
;;  <lisp>(planner-timeclock-report)</lisp>

(defun planner-timeclock-report ()
  (require 'timeclock)
  (with-temp-buffer
    (timeclock-generate-report emacs-wiki-publishing-p)
    (buffer-string)))

;;; planner.el ends here.

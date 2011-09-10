;;
;; This file contains all of the verbs and commands.
;;

(require 'cl)

(if nil
    (eval-and-compile (setq byte-compile-warnings nil)))

;;;; Give long description of room if haven't been there yet.  Otherwise
;;;; short.  Also give long if we were called with negative room number.

(defun describe-room (room)
  (if (and (not (member (abs room) light-rooms)) 
	   (not (member obj-lamp inventory)))
      (mprincl "It is pitch dark.  You are likely to be eaten by a grue.")
    (mprincl (cadr (nth (abs room) rooms)))
    (if (and (and (or (member room visited) 
		      (string= mode "superb")) (> room 0))
	     (not (string= mode "long")))
	nil
      (mprinc (car (nth (abs room) rooms)))
    (mprinc "\n"))
    (if (not (string= mode "long"))
	(if (not (member (abs room) visited))
	    (setq visited (append (list (abs room)) visited))))
    (dolist (xobjs (nth current-room room-objects))
      (if (= xobjs obj-special)
	  (special-object)
	(if (>= xobjs 0)
	    (mprincl (car (nth xobjs objects)))
	  (if (not (and (= xobjs obj-bus) inbus))
	      (progn
		(mprincl (car (nth (abs xobjs) perm-objects)))))))
      (if (and (= xobjs obj-jar) jar)
	  (progn
	    (mprincl "The jar contains:")
	    (dolist (x jar)
	      (mprinc "     ")
	      (mprincl (car (nth x objects)))))))
    (if (and (member obj-bus (nth current-room room-objects)) inbus)
	(mprincl "You are on the bus."))))

;;; There is a special object in the room.  This object's description,
;;; or lack thereof, depends on certain conditions.

(defun special-object ()
  (if (= current-room computer-room)
      (if computer
	  (mprincl 
"The panel lights are flashing in a seemingly organized pattern.")
	(mprincl "The panel lights are steady and motionless.")))

  (if (and (= current-room red-room) 
	   (not (member obj-towel (nth red-room room-objects))))
      (mprincl "There is a hole in the floor here."))

  (if (and (= current-room marine-life-area) black)
      (mprincl 
"The room is lit by a black light, causing the fish, and some of 
your objects, to give off an eerie glow."))
  (if (and (= current-room fourth-vermont-intersection) hole)
      (progn
	(mprincl"You fall into a hole in the ground.")
	(setq current-room vermont-station)
	(describe-room vermont-station)))

  (if (> current-room endgame-computer-room)
      (progn
	(if (not correct-answer)
	    (endgame-question)
	  (mprincl "Your question is:")
	  (mprincl endgame-question))))

  (if (= current-room sauna)
      (progn
	(mprincl (nth sauna-level '(
"It is normal room temperature in here."
"It is luke warm in here."
"It is comfortably hot in here."
"It is refreshingly hot in here."
"You are dead now.")))
	(if (and (= sauna-level 3) 
		 (or (member obj-rms inventory)
		     (member obj-rms (nth current-room room-objects))))
	    (progn
	      (mprincl 
"You notice the wax on your statuette beginning to melt, until it completely
melts off.  You are left with a beautiful diamond!")
	      (if (member obj-rms inventory)
		  (progn
		    (remove-obj-from-inven obj-rms)
		    (setq inventory (append inventory (list obj-diamond))))
		(remove-obj-from-room current-room obj-rms)
		(replace room-objects current-room
			 (append (nth current-room room-objects)
				 (list obj-diamond))))
	      (if (member obj-floppy inventory)
		  (progn
		    (mprincl
"You notice your floppy disk beginning to melt.  As you grab for it, the 
disk bursts into flames, and disintegrates.")
		    (remove-obj-from-inven obj-floppy)
		    (remove-obj-from-room current-room obj-floppy)))))))
)

;;;;;;;;;;;;;;;;;;;;;; Commands start here

(defun die (murderer)
  (mprinc "\n")
  (if murderer
      (mprincl "You are dead."))
  (do-logfile 'die murderer)
  (score nil)
  (setq dead t))

(defun quit (args)
  (die nil))

;; Print every object in player's inventory.  Special case for the jar,
;; as we must also print what is in it.

(defun inven (args)
  (mprinc "You currently have:")
  (mprinc "\n")
  (dolist (curobj inventory)
    (if curobj
	(progn
	  (mprincl (cadr (nth curobj objects)))
	  (if (and (= curobj obj-jar) jar)
	      (progn
		(mprincl "The jar contains:")
		(dolist (x jar)
		  (mprinc "     ")
		  (mprincl (cadr (nth x objects))))))))))

(defun shake (obj)
  (let (objnum)
    (when (setq objnum (objnum-from-args-std obj))
      (if (member objnum inventory)
	  (progn
;;;	If shaking anything will do anything, put here.
	    (mprinc "Shaking ")
	    (mprinc (downcase (cadr (nth objnum objects))))
	    (mprinc " seems to have no effect.")
	    (mprinc "\n")
	    )
	(if (and (not (member objnum (nth current-room room-silents)))
		 (not (member objnum (nth current-room room-objects))))
	    (mprincl "I don't see that here.")
;;;     Shaking trees can be deadly
	  (if (= objnum obj-tree)
	      (progn
		(mprinc
 "You begin to shake a tree, and notice a coconut begin to fall from the air.
As you try to get your hand up to block it, you feel the impact as it lands
on your head.")
		(die "a coconut"))
	    (if (= objnum obj-bear)
		(progn
		  (mprinc
"As you go up to the bear, it removes your head and places it on the ground.")
		  (die "a bear"))
	      (if (< objnum 0)
		  (mprincl "You cannot shake that.")
		(mprincl "You don't have that.")))))))))


(defun drop (obj)
  (if inbus
      (mprincl "You can't drop anything while on the bus.")
  (let (objnum ptr)
    (when (setq objnum (objnum-from-args-std obj))
      (if (not (setq ptr (member objnum inventory)))
	  (mprincl "You don't have that.")
	(progn
	  (remove-obj-from-inven objnum)
	  (replace room-objects current-room
		   (append (nth current-room room-objects)
			   (list objnum)))
	  (mprincl "Done.")
	  (if (member objnum (list obj-food obj-weight obj-jar))
	      (drop-check objnum))))))))

;; Dropping certain things causes things to happen.

(defun drop-check (objnum)
  (if (and (= objnum obj-food) (= room bear-hangout)
	   (member obj-bear (nth bear-hangout room-objects)))
      (progn
	(mprincl
"The bear takes the food and runs away with it. He left something behind.")
	(remove-obj-from-room current-room obj-bear)
	(remove-obj-from-room current-room obj-food)
	(replace room-objects current-room
		 (append (nth current-room room-objects)
			 (list obj-key)))))

  (if (and (= objnum obj-jar) (member obj-nitric jar) 
	   (member obj-glycerine jar))
      (progn
	(mprincl "As the jar impacts the ground it explodes into many pieces.")
	(setq jar nil)
	(remove-obj-from-room current-room obj-jar)
	(if (= current-room fourth-vermont-intersection)
	    (progn
	      (setq hole t)
	      (setq current-room vermont-station)
	      (mprincl 
"The explosion causes a hole to open up in the ground, which you fall
through.")))))

  (if (and (= objnum obj-weight) (= current-room maze-button-room))
      (mprincl "A passageway opens.")))

;;; Give long description of current room, or an object.
      
(defun examine (obj)
  (let (objnum)
    (setq objnum (objnum-from-args obj))
    (if (eq objnum obj-special)
	(describe-room (* current-room -1))
      (if (and (eq objnum obj-computer)
	       (member obj-pc (nth current-room room-silents)))
	  (examine '("pc"))
	(if (eq objnum nil)
	    (mprincl "I don't know what that is.")
	  (if (and (not (member objnum (nth current-room room-objects)))
		   (not (member objnum (nth current-room room-silents)))
		   (not (member objnum inventory)))
	      (mprincl "I don't see that here.")
	    (if (>= objnum 0)
		(if (and (= objnum obj-bone) 
			 (= current-room marine-life-area) black)
		    (mprincl 
"In this light you can see some writing on the bone.  It says:
For an explosive time, go to Fourth St. and Vermont.")
		  (if (nth objnum physobj-desc)
		      (mprincl (nth objnum physobj-desc))
		    (mprincl "I see nothing special about that.")))
	      (if (nth (abs objnum) permobj-desc)
		  (progn
		    (mprincl (nth (abs objnum) permobj-desc)))
		(mprincl "I see nothing special about that.")))))))))

(defun take (obj)
  (if inbus
      (mprincl "You can't take anything while on the bus.")
  (setq obj (firstword obj))
  (if (not obj)
      (mprincl "You must supply an object.")
    (if (string= obj "all")
	(let (gotsome)
	  (setq gotsome nil)
	  (dolist (x (nth current-room room-objects))
	    (if (and (>= x 0) (not (= x obj-special)))
		(progn
		  (setq gotsome t)
		  (mprinc (cadr (nth x objects)))
		  (mprinc ": ")
		  (take-object x))))
	  (if (not gotsome)
	      (mprincl "Nothing to take.")))
      (let (objnum)
	(setq objnum (cdr (assq (intern obj) objnames)))
	(if (eq objnum nil)
	    (progn
	      (mprinc "I don't know what that is.")
	      (mprinc "\n"))
	  (take-object objnum)))))))

(defun take-object (objnum)
  (if (and (member objnum jar) (member obj-jar inventory))
      (let (newjar)
	(mprincl "You remove it from the jar.")
	(setq newjar nil)
	(dolist (x jar)
	  (if (not (= x objnum))
	      (setq newjar (append newjar (list x)))))
	(setq jar newjar)
	(setq inventory (append inventory (list objnum))))
    (if (not (member objnum (nth current-room room-objects)))
	(if (not (member objnum (nth current-room room-silents)))
	    (mprinc "I do not see that here.")
	  (try-take objnum))
      (if (>= objnum 0)
	  (progn
	    (if (and (car inventory) 
		     (> (+ (inven-weight) (nth objnum object-lbs)) 11))
		(mprinc "Your load would be too heavy.")
	      (setq inventory (append inventory (list objnum)))
	      (remove-obj-from-room current-room objnum)
	      (mprinc "Taken.  ")
	      (if (and (= objnum obj-towel) (= current-room red-room))
		  (mprinc "Taking the towel reveals a hole in the floor."))))
	(try-take objnum)))
    (mprinc "\n")))

(defun inven-weight ()
  (let (total)
    (setq total 0)
    (dolist (x jar)
      (setq total (+ total (nth x object-lbs))))
    (dolist (x inventory)
      (setq total (+ total (nth x object-lbs)))) total))

;;; We try to take an object that is untakable.  Print a message
;;; depending on what it is.

(defun try-take (obj)
  (mprinc "You cannot take that."))

(defun dig (args)
  (if inbus
      (mprincl "You can't dig while on the bus.")
  (if (not (member 0 inventory))
      (mprincl "You have nothing with which to dig.")
    (if (not (nth current-room diggables))
	(mprincl "Digging here reveals nothing.")
      (mprincl "I think you found something.")
      (replace room-objects current-room
	       (append (nth current-room room-objects)
		       (nth current-room diggables)))
      (replace diggables current-room nil)))))

(defun climb (obj)
  (let (objnum)
    (setq objnum (objnum-from-args obj))
    (if (and (not (= objnum obj-special))
	     (not (member objnum (nth current-room room-objects)))
	     (not (member objnum (nth current-room room-silents)))
	     (not (member objnum inventory)))
	(mprincl "I don't see that here.")
      (if (and (= objnum obj-special)
	       (not (member obj-tree (nth current-room room-silents))))
	  (mprincl "There is nothing here to climb.")
	(if (and (not (= objnum obj-tree)) (not (= objnum obj-special)))
	    (mprincl "You can't climb that.")
	  (mprincl
"You manage to get about two feet up the tree and fall back down.  You
notice that the tree is very unsteady."))))))

(defun eat (obj)
  (let (objnum)
    (when (setq objnum (objnum-from-args-std obj))
      (if (not (member objnum inventory))
	  (mprincl "You don't have that.")
	(if (not (= objnum obj-food))
	    (progn
	      (mprinc "You forcefully shove ")
	      (mprinc (downcase (cadr (nth objnum objects))))
	      (mprincl " down your throat, and start choking.")
	      (die "choking"))
	  (mprincl "That tasted horrible.")
	  (remove-obj-from-inven obj-food))))))

(defun dput (args)
  (if inbus
      (mprincl "You can't do that while on the bus")
    (let (newargs objnum objnum2 obj)
      (setq newargs (firstwordl args))
      (if (not newargs)
	  (mprincl "You must supply an object")
	(setq obj (intern (car newargs)))
	(setq objnum (cdr (assq obj objnames)))
	(if (not objnum)
	    (mprincl "I don't know what that object is.")
	  (if (not (member objnum inventory))
	      (mprincl "You don't have that.")
	    (setq newargs (firstwordl (cdr newargs)))
	    (setq newargs (firstwordl (cdr newargs)))
	    (if (not newargs)
		(mprincl "You must supply an indirect object.")
	      (setq objnum2 (cdr (assq (intern (car newargs)) objnames)))
	      (if (and (eq objnum2 obj-computer) (= current-room pc-area))
		  (setq objnum2 obj-pc))
	      (if (not objnum2)
		  (mprincl "I don't know what that indirect object is.")
		(if (and (not (member objnum2 (nth current-room room-objects)))
			 (not (member objnum2 (nth current-room room-silents)))
			 (not (member objnum2 inventory)))
		    (mprincl "That indirect object is not here.")
		  (put-objs objnum objnum2))))))))))

(defun put-objs (obj1 obj2)
  (if (and (= obj2 obj-drop) (not nomail))
      (setq obj2 obj-chute))

  (if (= obj2 obj-disposal) (setq obj2 obj-chute))

  (if (and (= obj1 obj-cpu) (= obj2 obj-computer))
      (progn
	(remove-obj-from-inven obj-cpu)
	(setq computer t)
	(mprincl
"As you put the CPU board in the computer, it immediately springs to life.
The lights start flashing, and the fans seem to startup."))
    (if (and (= obj1 obj-weight) (= obj2 obj-button))
	(drop '("weight"))
      (if (= obj2 obj-jar)                 ;; Put something in jar
	  (if (not (member obj1 (list obj-paper obj-diamond obj-emerald
				      obj-license obj-coins obj-egg
				      obj-nitric obj-glycerine)))
	      (mprincl "That will not fit in the jar.")
	    (remove-obj-from-inven obj1)
	    (setq jar (append jar (list obj1)))
	    (mprincl "Done."))
	(if (= obj2 obj-chute)                 ;; Put something in chute
	    (progn
	      (remove-obj-from-inven obj1)
	      (mprincl 
"You hear it slide down the chute and off into the distance.")
	      (put-objs-in-treas (list obj1)))
	  (if (= obj2 obj-box)              ;; Put key in key box
	      (if (= obj1 obj-key)
		  (progn
		    (mprincl
"As you drop the key, the box begins to shake.  Finally it explodes
with a bang.  The key seems to have vanished!")
		    (remove-obj-from-inven obj1)
		    (replace room-objects computer-room (append
							(nth computer-room
							     room-objects)
							(list obj1)))
		    (remove-obj-from-room current-room obj-box)
		    (setq key-level (1+ key-level)))
		(mprincl "You can't put that in the key box!"))

	    (if (and (= obj1 obj-floppy) (= obj2 obj-pc))
		(progn
		  (setq floppy t)
		  (remove-obj-from-inven obj1)
		  (mprincl "Done."))

	      (if (= obj2 obj-urinal)                   ;; Put object in urinal
		  (progn
		    (remove-obj-from-inven obj1)
		    (replace room-objects urinal (append 
						  (nth urinal room-objects)
						   (list obj1)))
		    (mprincl
		     "You hear it plop down in some water below."))
		(if (= obj2 obj-mail)
		    (mprincl "The mail chute is locked.")
		  (if (member obj1 inventory)
		      (mprincl 
"I don't know how to combine those objects.  Perhaps you should
just try dropping it.")
		    (mprincl"You can't put that there.")))))))))))

(defun type (args)
  (if (not (= current-room computer-room))
      (mprincl "There is nothing here on which you could type.")
    (if (not computer)
	(mprincl 
"You type on the keyboard, but your characters do not even echo.")
      (unix-interface))))

;;;; Various movement directions

(defun n (args)
  (move north))

(defun s (args)
  (move south))

(defun e (args)
  (move east))

(defun w (args)
  (move west))

(defun ne (args)
  (move northeast))

(defun se (args)
  (move southeast))

(defun nw (args)
  (move northwest))

(defun sw (args)
  (move southwest))

(defun up (args)
  (move up))

(defun down (args)
  (move down))

(defun in (args)
  (move in))

(defun out (args)
  (move out))

(defun go (args)
  (if (or (not (car args)) 
	  (eq (doverb ignore verblist (car args) (cdr (cdr args))) -1))
      (mprinc "I don't understand where you want me to go.\n")))

;; Uses the dungeon-map to figure out where we are going.  If the
;; requested direction yields 255, we know something special is
;; supposed to happen, or perhaps you can't go that way unless
;; certain conditions are met.

(defun move (dir)
  (if (and (not (member current-room light-rooms)) 
	   (not (member obj-lamp inventory)))
      (progn
	(mprinc 
"You trip over a grue and fall into a pit and break every bone in your
body.")
	(die "a grue"))
    (let (newroom)
      (setq newroom (nth dir (nth current-room dungeon-map)))
      (if (eq newroom -1)
	  (mprinc "You can't go that way.\n")
	(if (eq newroom 255)
	    (special-move dir)
	  (setq room -1)
	  (setq lastdir dir)
	  (if inbus
	      (progn
		(if (or (< newroom 58) (> newroom 83))
		    (mprincl "The bus cannot go this way.")
		  (mprincl 
		   "The bus lurches ahead and comes to a screeching halt.")
		  (remove-obj-from-room current-room obj-bus)
		  (setq current-room newroom)
		  (replace room-objects newroom
			   (append (nth newroom room-objects)
				   (list obj-bus)))))
	    (setq current-room newroom)))))))

;; Movement in this direction causes something special to happen if the
;; right conditions exist.  It may be that you can't go this way unless
;; you have a key, or a passage has been opened.

;; coding note: Each check of the current room is on the same 'if' level,
;; i.e. there aren't else's.  If two rooms next to each other have
;; specials, and they are connected by specials, this could cause
;; a problem.  Be careful when adding them to consider this, and
;; perhaps use else's.

(defun special-move (dir)
  (if (= current-room building-front)
      (if (not (member obj-key inventory))
	  (mprincl "You don't have a key that can open this door.")
	(setq current-room old-building-hallway))
    (if (= current-room north-end-of-cave-passage)
	(let (combo)
	  (mprincl 
"You must type a 3 digit combination code to enter this room.")
	  (mprinc "Enter it here: ")
	  (setq combo (read-line))
	  (if (not batch-mode)
	      (mprinc "\n"))
	  (if (string= combo combination)
	      (setq current-room gamma-computing-center)
	    (mprincl "Sorry, that combination is incorrect."))))

    (if (= current-room bear-hangout)
	(if (member obj-bear (nth bear-hangout room-objects))
	    (progn
	      (mprinc 
"The bear is very annoyed that you would be so presumptuous as to try
and walk right by it.  He tells you so by tearing your head off.
")
	      (die "a bear"))
	  (mprincl "You can't go that way.")))

    (if (= current-room vermont-station)
	(progn
	  (mprincl
"As you board the train it immediately leaves the station.  It is a very
bumpy ride.  It is shaking from side to side, and up and down.  You
sit down in one of the chairs in order to be more comfortable.")
	  (mprincl
"\nFinally the train comes to a sudden stop, and the doors open, and some
force throws you out.  The train speeds away.\n")
	  (setq current-room museum-station)))

    (if (= current-room old-building-hallway)
	(if (and (member obj-key inventory)
		 (> key-level 0))
	    (setq current-room meadow)
	  (mprincl "You don't have a key that can open this door.")))

    (if (and (= current-room maze-button-room) (= dir northwest))
	(if (member obj-weight (nth maze-button-room room-objects))
	    (setq current-room 18)
	  (mprincl "You can't go that way.")))

    (if (and (= current-room maze-button-room) (= dir up))
	(if (member obj-weight (nth maze-button-room room-objects))
	    (mprincl "You can't go that way.")
	  (setq current-room weight-room)))

    (if (= current-room classroom)
	(mprincl "The door is locked."))

    (if (or (= current-room lakefront-north) (= current-room lakefront-south))
	(swim nil))

    (if (= current-room reception-area)
	(if (not (= sauna-level 3))
	    (setq current-room health-club-front)
	  (mprincl
"As you exit the building, you notice some flames coming out of one of the
windows.  Suddenly, the building explodes in a huge ball of fire.  The flames
engulf you, and you burn to death.")
	  (die "burning")))

    (if (= current-room red-room)
	(if (not (member obj-towel (nth red-room room-objects)))
	    (setq current-room long-n-s-hallway)
	  (mprincl "You can't go that way.")))

    (if (and (> dir down) (> current-room gamma-computing-center) 
	     (< current-room museum-lobby))
	(if (not (member obj-bus (nth current-room room-objects)))
	    (mprincl "You can't go that way.")
	  (if (= dir in)
	      (if (member obj-license inventory)
		  (progn
		    (mprincl "You board the bus and get in the driver's seat.")
		    (setq nomail t)
		    (setq inbus t))
		(mprincl "You are not licensed for this type of vehicle."))
	    (mprincl "You hop off the bus.")
	    (setq inbus nil)))
      (if (= current-room fifth-oaktree-intersection)
	  (if (not inbus)
	      (progn
		(mprincl "You fall down the cliff and land on your head.")
		(die "a cliff"))
	    (mprincl
"The bus flies off the cliff, and plunges to the bottom, where it explodes.")
	    (die "a bus accident")))
      (if (= current-room main-maple-intersection)
	  (progn
	    (if (not inbus)
		(mprincl "The gate will not open.")
	      (mprincl
"As the bus approaches, the gate opens and you drive through.")
	      (remove-obj-from-room main-maple-intersection obj-bus)
	      (replace room-objects museum-entrance 
		       (append (nth museum-entrance room-objects)
			       (list obj-bus)))
	      (setq current-room museum-entrance)))))
    (if (= current-room cave-entrance)
	(progn
	  (mprincl
"As you enter the room you hear a rumbling noise.  You look back to see
huge rocks sliding down from the ceiling, and blocking your way out.\n")
	  (setq current-room misty-room)))))

(defun long (args)
  (setq mode "long"))

(defun turn (obj)
  (let (objnum direction)
    (when (setq objnum (objnum-from-args-std obj))
      (if (not (or (member objnum (nth current-room room-objects))
		   (member objnum (nth current-room room-silents))))
	  (mprincl "I don't see that here.")
	(if (not (= objnum obj-dial))
	    (mprincl "You can't turn that.")
	  (setq direction (firstword (cdr obj)))
	  (if (or (not direction) 
		  (not (or (string= direction "clockwise")
			   (string= direction "counterclockwise"))))
	      (mprincl "You must indicate clockwise or counterclockwise.")
	    (if (string= direction "clockwise")
		(setq sauna-level (+ sauna-level 1))
	      (setq sauna-level (- sauna-level 1)))
	    
	    (if (< sauna-level 0)
		(progn
		  (mprincl 
		   "The dial will not turn further in that direction.")
		  (setq sauna-level 0))
	      (sauna-heat))))))))

(defun sauna-heat ()
  (if (= sauna-level 0)
      (mprincl "The termperature has returned to normal room termperature."))
  (if (= sauna-level 1)
      (mprincl "It is now luke warm in here.  You begin to sweat."))
  (if (= sauna-level 2)
      (mprincl "It is pretty hot in here.  It is still very comfortable."))
  (if (= sauna-level 3)
      (progn
	(mprincl 
"It is now very hot.  There is something very refreshing about this.")
	(if (or (member obj-rms inventory) 
		(member obj-rms (nth current-room room-objects)))
	    (progn
	      (mprincl 
"You notice the wax on your statuette beginning to melt, until it completely
melts off.  You are left with a beautiful diamond!")
	      (if (member obj-rms inventory)
		  (progn
		    (remove-obj-from-inven obj-rms)
		    (setq inventory (append inventory (list obj-diamond))))
		(remove-obj-from-room current-room obj-rms)
		(replace room-objects current-room
			 (append (nth current-room room-objects)
				 (list obj-diamond))))))
	(if (or (member obj-floppy inventory)
		(member obj-floppy (nth current-room room-objects)))
	    (progn
	      (mprincl
"You notice your floppy disk beginning to melt.  As you grab for it, the 
disk bursts into flames, and disintegrates.")
	      (if (member obj-floppy inventory)
		  (remove-obj-from-inven obj-floppy)
		(remove-obj-from-room current-room obj-floppy))))))

  (if (= sauna-level 4)
      (progn
	(mprincl 
"As the dial clicks into place, you immediately burst into flames.")
	(die "burning"))))

(defun press (obj)
  (let (objnum)
    (when (setq objnum (objnum-from-args-std obj))
      (if (not (or (member objnum (nth current-room room-objects))
		   (member objnum (nth current-room room-silents))))
	  (mprincl "I don't see that here.")
	(if (not (member objnum (list obj-button obj-switch)))
	    (progn
	      (mprinc "You can't ")
	      (mprinc (car line-list))
	      (mprincl " that."))
	  (if (= objnum obj-button)
	      (mprincl
"As you press the button, you notice a passageway open up, but
as you release it, the passageway closes."))
	  (if (= objnum obj-switch)
	      (if black
		  (progn
		    (mprincl "The button is now in the off position.")
		    (setq black nil))
		(mprincl "The button is now in the on position.")
		(setq black t))))))))

(defun swim (args)
  (if (not (member current-room (list lakefront-north lakefront-south)))
      (mprincl "I see no water!")
    (if (not (member obj-life inventory))
	(progn
	  (mprincl 
"You dive in the water, and at first notice it is quite cold.  You then
start to get used to it as you realize that you never really learned how
to swim.")
	  (die "drowning"))
      (if (= current-room lakefront-north)
	  (setq current-room lakefront-south)
	(setq current-room lakefront-north)))))


(defun score (args)
  (if (not endgame)
      (let (total)
	(setq total (reg-score))
	(mprinc "You have scored ")
	(mprinc total)
	(mprincl " out of a possible 90 points.") total)
    (mprinc "You have scored ")
    (mprinc (endgame-score))
    (mprincl " endgame points out of a possible 110.")
    (if (= (endgame-score) 110)
	(mprincl 
"\n\nCongratulations.  You have won.  The wizard password is 'moby'"))))

(defun help (args)
  (mprincl
"Welcome to dunnet (2.0), by Ron Schnell (ronnie@media.mit.edu).
Here is some useful information (read carefully because there are one
or more clues in here):

- If you have a key that can open a door, you do not need to explicitly
  open it.  You may just use 'in' or walk in the direction of the door.

- If you have a lamp, it is always lit.

- You will not get any points until you manage to get treasures to a certain
  place.  Simply finding the treasures is not good enough.  There is more
  than one way to get a treasure to the special place.  It is also
  important that the objects get to the special place *unharmed* and
  *untarnished*.  You can tell if you have successfully transported the
  object by looking at your score, as it changes immediately.  Note that
  an object can become harmed even after you have received points for it.
  If this happens, your score will decrease, and in many cases you can never
  get credit for it again.

- You can save your game with the 'save' command, and use restore it
  with the 'restore' command.

- There are no limits on lengths of object names.

- Directions are: north,south,east,west,northeast,southeast,northwest,
                  southwest,up,down,in,out.

- These can be abbreviated: n,s,e,w,ne,se,nw,sw,u,d,in,out.

- If you go down a hole in the floor without an aid such as a ladder,
  you probably won't be able to get back up the way you came, if at all.

- It is possible to get the maximum points.

If you have questions or comments, contact ronnie@media.mit.edu."))

(defun flush (args)
  (if (not (= current-room bathroom))
      (mprincl "I see nothing to flush.")
    (mprincl "Whoooosh!!")
    (put-objs-in-treas (nth urinal room-objects))
    (replace room-objects urinal nil)))

(defun piss (args)
  (if (not (= current-room bathroom))
      (mprincl "You can't do that here, don't even bother trying.")
    (if (not gottago)
	(mprincl "I'm afraid you don't have to go now.")
      (mprincl "That was refreshing.")
      (setq gottago nil)
      (replace room-objects urinal (append (nth urinal room-objects)
					   (list obj-URINE))))))


(defun dsleep (args)
  (if (not (= current-room bedroom))
      (mprincl
"You try to go to sleep while standing up here, but can't seem to do it.")
    (setq gottago t)
    (mprincl
"As soon as you start to doze off you begin dreaming.  You see images of
workers digging caves, slaving in the humid heat.  Then you see yourself
as one of these workers.  While no one is looking, you leave the group
and walk into a room.  The room is bare except for a horseshoe
shaped piece of stone in the center.  You see yourself digging a hole in
the ground, then putting some kind of treasure in it, and filling the hole
with dirt again.  After this, you immediately wake up.")))

(defun break (obj)
  (let (objnum)
    (if (not (member obj-axe inventory))
	(mprincl "You have nothing you can use to break things.")
      (when (setq objnum (objnum-from-args-std obj))
	(if (member objnum inventory)
	    (progn
	      (mprincl
"You take the object in your hands and swing the axe.  Unfortunately, you miss
the object and slice off your hand.  You bleed to death.")
	      (die "an axe"))
	  (if (not (or (member objnum (nth current-room room-objects))
		       (member objnum (nth current-room room-silents))))
	      (mprincl "I don't see that here.")
	    (if (= objnum obj-cable)
		(progn
		  (mprincl 
"As you break the ethernet cable, everything starts to blur.  You collapse
for a moment, then straighten yourself up.
")
		  (replace room-objects gamma-computing-center
			   (append (nth gamma-computing-center room-objects)
				   inventory))
		  (if (member obj-key inventory)
		      (progn
			(setq inventory (list obj-key))
			(remove-obj-from-room gamma-computing-center obj-key))
		    (setq inventory nil))
		  (setq current-room computer-room)
		  (setq ethernet nil)
		  (mprincl "Connection closed.")
		  (unix-interface))
	      (if (< objnum 0)
		  (progn
		    (mprincl "Your axe shatters into a million pieces.")
		    (remove-obj-from-inven obj-axe))
		(mprincl "Your axe breaks it into a million pieces.")
		(remove-obj-from-room current-room objnum)))))))))

(defun drive (args)
  (if (not inbus)
      (mprincl "You cannot drive when you aren't in a vehicle.")
    (mprincl "To drive while you are in the bus, just give a direction.")))

(defun superb (args)
  (setq mode 'superb))

(defun reg-score ()
  (let (total)
    (setq total 0)
    (dolist (x (nth treasure-room room-objects))
      (setq total (+ total (nth x object-pts))))
    (if (member obj-URINE (nth treasure-room room-objects))
	(setq total 0)) total))

(defun endgame-score ()
  (let (total)
    (setq total 0)
    (dolist (x (nth endgame-treasure-room room-objects))
      (setq total (+ total (nth x object-pts)))) total))

(defun answer (args)
  (if (not correct-answer)
      (mprincl "I don't believe anyone asked you anything.")
    (setq args (car args))
    (if (not args)
	(mprincl "You must give the answer on the same line.")
      (if (members args correct-answer)
	  (progn
	    (mprincl "Correct.")
	    (if (= lastdir 0)
		(setq current-room (1+ current-room))
	      (setq current-room (- current-room 1)))
	    (setq correct-answer nil))
	(mprincl "That answer is incorrect.")))))

(defun endgame-question ()
(if (not endgame-questions)
    (progn
      (mprincl "Your question is:")
      (mprincl "No more questions, just do 'answer foo'.")
      (setq correct-answer '("foo")))
  (let (which i newques)
    (setq i 0)
    (setq newques nil)
    (setq which (% (abs (random)) (length endgame-questions)))
    (mprincl "Your question is:")
    (mprincl (setq endgame-question (car (nth which endgame-questions))))
    (setq correct-answer (cdr (nth which endgame-questions)))
    (while (< i which)
      (setq newques (append newques (list (nth i endgame-questions))))
      (setq i (1+ i)))
    (setq i (1+ which))
    (while (< i (length endgame-questions))
      (setq newques (append newques (list (nth i endgame-questions))))
      (setq i (1+ i)))
    (setq endgame-questions newques))))

(defun dun-power (args)
  (if (not (= current-room pc-area))
      (mprincl "That operation is not applicable here.")
    (if (not floppy)
	(dos-no-disk)
      (dos-interface))))

(defun touka (args)
  (setq current-room computer-room)
  (setq logged-in t)
  (setq computer t))

(defun dun-feed (args)
  (let (objnum)
    (when (setq objnum (objnum-from-args-std args))
      (if (and (= objnum obj-bear) 
	       (member obj-bear (nth current-room room-objects)))
	  (progn
	    (if (not (member obj-food inventory))
		(mprincl "You have nothing with which to feed it.")
	      (drop '("food"))))
	(if (not (or (member objnum (nth current-room room-objects))
		     (member objnum inventory)
		     (member objnum (nth current-room room-silents))))
	    (mprincl "I don't see that here.")
	  (mprincl "You cannot feed that."))))))

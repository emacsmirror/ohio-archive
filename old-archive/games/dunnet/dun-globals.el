;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;       Globals
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; IMPORTANT
;;;; All globals which can change must be saved from 'save-game.  Add
;;;; all new globals to bottom of file.

(if nil
    (eval-and-compile (setq byte-compile-warnings nil)))
(setq visited '(27))
(setq current-room 1)
(setq exitf nil)
(setq badcd nil)
(defvar dungeon-mode-map nil)
(setq dungeon-mode-map (make-sparse-keymap))
(define-key dungeon-mode-map "\r" 'dungeon-parse)
(defvar dungeon-batch-map (make-keymap))
(if (string= (substring emacs-version 0 2) "18")
    (let (n)
      (setq n 32)
      (while (< 0 (setq n (- n 1)))
	(aset dungeon-batch-map n 'dungeon-nil)))
  (let (n)
    (setq n 32)
    (while (< 0 (setq n (- n 1)))
      (aset (car (cdr dungeon-batch-map)) n 'dungeon-nil))))
(define-key dungeon-batch-map "\r" 'exit-minibuffer)
(define-key dungeon-batch-map "\n" 'exit-minibuffer)
(setq computer nil)
(setq floppy nil)
(setq door1 'locked)
(setq key-level 0)
(setq hole nil)
(setq correct-answer nil)
(setq lastdir 0)
(setq numsaves 0)
(setq jar nil)
(setq numcmds 0)
(setq wizard nil)
(setq endgame-question nil)
(setq logged-in nil)
(setq dungeon-mode 'dungeon)
(setq unix-verbs '((ls . ls) (ftp . ftp) (echo . echo) (exit . uexit)
		   (cd . dunnet-cd) (pwd . dunnet-pwd) (rlogin . rlogin)
		   (uncompress . uncompress) (cat . cat) (zippy . zippy)))

(setq dos-verbs '((dir . dos-dir) (type . dos-type) (exit . dos-exit)
		  (command . dos-spawn) (b: . dos-invd) (c: . dos-invd)
		  (a: . dos-nil)))


(setq batch-mode nil)

(setq cdpath "/usr/toukmond")
(setq cdroom -10)
(setq uncompressed nil)
(setq ethernet t)
(setq restricted '(room-objects dungeon-map rooms room-silents combination))
(setq path "/usr/toukmond")
(setq ftptype 'ascii)
(setq endgame nil)
(setq gottago t)
(setq black nil)

(setq rooms '(
	      (
"You are in the treasure room.  A door leads out to the north."
               "Treasure room"
	       )
	      (
"You are at a dead end of a dirt road.  The road goes to the east.
In the distance you can see that it will eventually fork off.  The
trees here are very tall royal palms, and they are spaced equidistant
from each other."
	       "Dead end"
	       )
	      (
"You are on the continuation of a dirt road.  There are more trees on
both sides of you.  The road continues to the east and west."
               "E/W Dirt road"
	       )
	      (
"You are at a fork of two passages, one to the northeast, and one to the
southeast.  The ground here seems very soft. You can also go back west."
               "Fork"
	       )
	      (
"You are on a northeast/southwest road."
               "NE/SW road"
	       )
	      (
"You are at the end of the road.  There is a building in front of you
to the northeast, and the road leads back to the southwest."
               "Building front"
	       )
	      (
"You are on a southeast/northwest road."
               "SE/NW road"
	       )
	      (
"You are standing at the end of a road.  A passage leads back to the
northwest."
               "Bear hangout"
	       )
	      (
"You are in the hallway of an old building.  There are rooms to the east
and west, and doors leading out to the north and south."
               "Old Building hallway"
	       )
	      (
"You are in a mailroom.  There are many bins where the mail is usually
kept.  The exit is to the west."
               "Mailroom"
	       )
	      (
"You are in a computer room.  It seems like most of the equipment has
been removed.  There is a VAX 11/780 in front of you, however, with
one of the cabinets wide open.  A sign on the front of the machine
says: This VAX is named 'pokey'.  To type on the console, use the
'type' command.  The exit is to the east."
               "Computer room"
	       )
	      (
"You are in a meadow in the back of an old building.  A small path leads
to the west, and a door leads to the south."
               "Meadow"
	       )
	      (
"You are in a round, stone room with a door to the east.  There
is a sign on the wall that reads: 'receiving room'."
               "Receiving room"
	       )
	      (
"You are at the south end of a hallway that leads to the north.  There
are rooms to the east and west."
               "Northbound Hallway"
	       )
	      (
"You are in a sauna.  There is nothing in the room except for a dial
on the wall.  A door leads out to west."
               "Sauna"
               )
	      (
"You are at the end of a north/south hallway.  You can go back to the south,
or off to a room to the east."
               "End of N/S Hallway"
	       )
	      (
"You are in an old weight room.  All of the equipment is either destroyed
or completely broken.  There is a door out to the west, and there is a ladder
leading down a hole in the floor."
               "Weight room"                 ;16
	       )
	      (
"You are in a maze of twisty little passages, all alike.
There is a button on the ground here."
               "Maze button room"
	       )
	      (
"You are in a maze of little twisty passages, all alike."
               "Maze"
	       )
	      (
"You are in a maze of thirsty little passages, all alike."
               "Maze"    ;19
	       )
	      (
"You are in a maze of twenty little passages, all alike."
               "Maze"
	       )
	      (
"You are in a daze of twisty little passages, all alike."
               "Maze"   ;21
	       )
	      (
"You are in a maze of twisty little cabbages, all alike."
               "Maze"   ;22
	       )
	      (
"You are in a reception area for a health and fitness center.  The place
appears to have been recently ransacked, and nothing is left.  There is
a door out to the south, and a crawlspace to the southeast."
               "Reception area"
	       )
	      (
"You are outside a large building to the north which used to be a health
and fitness center.  A road leads to the south."
               "Health Club front"
	       )
	      (
"You are at the north side of a lake.  On the other side you can see
a road which leads to a cave.  The water appears very deep."
               "Lakefront North"
	       )
	      (
"You are at the south side of a lake.  A road goes to the south."
               "Lakefront South"
	       )
	      (
"You are in a well-hidden area off to the side of a road.  Back to the
northeast through the brush you can see the bear hangout."
               "Hidden area"
	       )
	      (
"The entrance to a cave is to the south.  To the north, a road leads
towards a deep lake.  On the ground nearby there is a chute, with a sign
that says 'put treasures here for points'."
               "Cave Entrance"                      ;28
	       )
	      (
"You are in a misty, humid room carved into a mountain.
To the north is the remains of a rockslide.  To the east, a small
passage leads away into the darkness."              ;29
               "Misty Room"
	       )
	      (
"You are in an east/west passageway.  The walls here are made of
multicolored rock and are quite beautiful."
               "Cave E/W passage"                   ;30
	       )
	      (
"You are at the junction of two passages. One goes north/south, and
the other goes west."
               "N/S/W Junction"                     ;31
	       )
	      (
"You are at the north end of a north/south passageway.  There are stairs
leading down from here.  There is also a door leading west."
               "North end of cave passage"         ;32
	       )
	      (
"You are at the south end of a north/south passageway.  There is a hole
in the floor here, into which you could probably fit."
               "South end of cave passage"         ;33
	       )
	      (
"You are in what appears to be a worker's bedroom.  There is a queen-
sized bed in the middle of the room, and a painting hanging on the
wall.  A door leads to another room to the south, and stairways
lead up and down."
               "Bedroom"                          ;34
	       )
	      (
"You are in a bathroom built for workers in the cave.  There is a
urinal hanging on the wall, and some exposed pipes on the opposite
wall where a sink used to be.  To the north is a bedroom."
               "Bathroom"        ;35
	       )
	      (
"This is a marker for the urinal.  User will not see this, but it
is a room that can contain objects."
               "Urinal"          ;36
	       )
	      (
"You are at the northeast end of a northeast/southwest passageway.
Stairs lead up out of sight."
               "Ne end of ne/sw cave passage"       ;37
	       )
	      (
"You are at the junction of northeast/southwest and east/west passages."
               "Ne/sw-e/w junction"                      ;38
	       )
	      (
"You are at the southwest end of a northeast/southwest passageway."
               "Sw end of ne/sw cave passage"        ;39
	       )
	      (
"You are at the east end of an e/w passage.  There are stairs leading up
to a room above."
               "East end of e/w cave passage"    ;40
	       )
	      (
"You are at the west end of an e/w passage.  There is a hole on the ground
which leads down out of sight."
               "West end of e/w cave passage"    ;41
	       )
	      (
"You are in a room which is bare, except for a horseshoe shaped boulder
in the center.  Stairs lead down from here."     ;42
               "Horseshoe boulder room"
	       )
	      (
"You are in a room which is completely empty.  Doors lead out to the north
and east."
               "Empty room"                      ;43
	       )
	      (
"You are in an empty room.  Interestingly enough, the stones in this
room are painted blue.  Doors lead out to the east and south."  ;44
               "Blue room"
	       )
	      (
"You are in an empty room.  Interestingly enough, the stones in this
room are painted yellow.  Doors lead out to the south and west."    ;45
               "Yellow room"
	       )
	      (
"You are in an empty room.  Interestingly enough, the stones in this room
are painted red.  Doors lead out to the west and north."
               "Red room"                                 ;46
	       )
	      (
"You are in the middle of a long north/south hallway."     ;47
               "Long n/s hallway"
	       )
	      (
"You are 3/4 of the way towards the north end of a long north/south hallway."
               "3/4 north"                                ;48
	       )
	      (
"You are at the north end of a long north/south hallway.  There are stairs
leading upwards."
               "North end of long hallway"                 ;49
	       )
	      (
"You are 3/4 of the way towards the south end of a long north/south hallway."
               "3/4 south"                                 ;50
	       )
	      (
"You are at the south end of a long north/south hallway.  There is a hole
to the south."
               "South end of long hallway"                 ;51
	       )
	      (
"You are at a landing in a stairwell which continues up and down."
               "Stair landing"                             ;52
	       )
	      (
"You are at the continuation of an up/down staircase."
               "Up/down staircase"                         ;53
	       )
	      (
"You are at the top of a staircase leading down.  A crawlway leads off
to the northeast."
               "Top of staircase."                        ;54
	       )
	      (
"You are in a crawlway that leads northeast or southwest."
               "Ne crawlway"                              ;55
	       )
	      (
"You are in a small crawlspace.  There is a hole in the ground here, and
a small passage back to the southwest."
               "Small crawlspace"                         ;56
	       )
	      (
"You are in the Gamma Computing Center.  An IBM 3090/600s is whirring
away in here.  There is an ethernet cable coming out of one of the units,
and going through the ceiling.  There is no console here on which you
could type."
               "Gamma computing center"                   ;57
	       )
	      (
"You are near the remains of a post office.  There is a mail drop on the
face of the building, but you cannot see where it leads.  A path leads
back to the east, and a road leads to the north."
               "Post office"                             ;58
	       )
	      (
"You are at the intersection of Main Street and Maple Ave.  Main street
runs north and south, and Maple Ave runs east off into the distance.
If you look north and east you can see many intersections, but all of
the buildings that used to stand here are gone.  Nothing remains except
street signs.
There is a road to the northwest leading to a gate that guards a building."
               "Main-Maple intersection"                       ;59
	       )
	      (
"You are at the intersection of Main Street and the west end of Oaktree Ave."
               "Main-Oaktree intersection"   ;60
	       )
	      (
"You are at the intersection of Main Street and the west end of Vermont Ave."
               "Main-Vermont intersection"  ;61
	       )
	      (
"You are at the north end of Main Street at the west end of Sycamore Ave." ;62
               "Main-Sycamore intersection"
	       )
	      (
"You are at the south end of First Street at Maple Ave." ;63
               "First-Maple intersection"
	       )
	      (
"You are at the intersection of First Street and Oaktree Ave."  ;64
               "First-Oaktree intersection"
	       )
	      (
"You are at the intersection of First Street and Vermont Ave."  ;65
               "First-Vermont intersection"
	       )
	      (
"You are at the north end of First Street at Sycamore Ave."  ;66
               "First-Sycamore intersection"
	       )
	      (
"You are at the south end of Second Street at Maple Ave."  ;67
               "Second-Maple intersection"
	       )
	      (
"You are at the intersection of Second Street and Oaktree Ave."  ;68
               "Second-Oaktree intersection"
	       )
	      (
"You are at the intersection of Second Street and Vermont Ave."  ;69
               "Second-Vermont intersection"
	       )
	      (
"You are at the north end of Second Street at Sycamore Ave."  ;70
               "Second-Sycamore intersection"
	       )
	      (
"You are at the south end of Third Street at Maple Ave."  ;71
               "Third-Maple intersection"
	       )
	      (
"You are at the intersection of Third Street and Oaktree Ave."  ;72
               "Third-Oaktree intersection"
	       )
	      (
"You are at the intersection of Third Street and Vermont Ave."  ;73
               "Third-Vermont intersection"
	       )
	      (
"You are at the north end of Third Street at Sycamore Ave."  ;74
               "Third-Sycamore intersection"
	       )
	      (
"You are at the south end of Fourth Street at Maple Ave."  ;75
               "Fourth-Maple intersection"
	       )
	      (
"You are at the intersection of Fourth Street and Oaktree Ave."  ;76
               "Fourth-Oaktree intersection"
	       )
	      (
"You are at the intersection of Fourth Street and Vermont Ave."  ;77
               "Fourth-Vermont intersection"
	       )
	      (
"You are at the north end of Fourth Street at Sycamore Ave."  ;78
               "Fourth-Sycamore intersection"
	       )
	      (
"You are at the south end of Fifth Street at the east end of Maple Ave."  ;79
               "Fifth-Maple intersection"
	       )
	      (
"You are at the intersection of Fifth Street and the east end of Oaktree Ave.
There is a cliff off to the east."
               "Fifth-Oaktree intersection"  ;80
	       )
	      (
"You are at the intersection of Fifth Street and the east end of Vermont Ave."
               "Fifth-Vermont intersection"  ;81
	       )
	      (
"You are at the north end of Fifth Street and the east end of Sycamore Ave."
               "Fifth-Sycamore intersection"  ;82
	       )
	      (
"You are in front of the Museum of Natural History.  A door leads into
the building to the north, and a road leads to the southeast."
               "Museum entrance"                  ;83
	       )
	      (
"You are in the main lobby for the Museum of Natural History.  In the center
of the room is the huge skeleton of a dinosaur.  Doors lead out to the
south and east." 
               "Museum lobby"                     ;84
	       )
	      (
"You are in the geological display.  All of the objects that used to
be on display are missing.  There are rooms to the east, west, and 
north."
               "Geological display"               ;85
	       )
	      (
"You are in the marine life area.  The room is filled with fish tanks,
which are filled with dead fish that have apparently died due to
starvation.  Doors lead out to the south and east."
               "Marine life area"                   ;86
	       )
	      (
"You are in some sort of maintenance room for the museum.  There is a
switch on the wall labeled 'BL'.  There are doors to the west and north."
               "Maintenance room"                   ;87
	       )
	      (
"You are in a classroom where school children were taught about natural
history.  On the blackboard is written, 'No children allowed downstairs.'
There is a door to the east with an 'exit' sign on it.  There is another
door to the west."
               "Classroom"                          ;88
	       )
	      (
"You are at the Vermont St. subway station.  A train is sitting here waiting."
               "Vermont station"                    ;89
	       )
	      (
"You are at the Museum subway stop.  A passage leads off to the north."
               "Museum station"                     ;90
	       )
	      (
"You are in a north/south tunnel."
               "N/S tunnel"                          ;91
	       )
	      (
"You are at the north end of a north/south tunnel.  Stairs lead up and
down from here.  There is a garbage disposal here."
               "North end of n/s tunnel"             ;92
               )
	      (
"You are at the top of some stairs near the subway station.  There is
a door to the west."
               "Top of subway stairs"           ;93
	       )
	      (
"You are at the bottom of some stairs near the subway station.  There is
a room to the northeast."
               "Bottom of subway stairs"       ;94
	       )
	      (
"You are in another computer room.  There is a computer in here larger
than you have ever seen.  It has no manufacturers name on it, but it
does have a sign that says: This machine's name is 'endgame'.  The
exit is to the southwest.  There is no console here on which you could
type."
               "Endgame computer room"         ;95
	       )
	      (
"You are in a north/south hallway."
               "Endgame n/s hallway"           ;96
	       )
	      (
"You have reached a question room.  You must answer a question correctly in
order to get by.  Use the 'answer' command to answer the question."
               "Question room 1"              ;97
	       )
	      (
"You are in a north/south hallway."
               "Endgame n/s hallway"           ;98
	       )
	      (
"You are in a second question room."
               "Question room 2"               ;99
	       )
	      (
"You are in a north/south hallway."
               "Endgame n/s hallway"           ;100
	       )
	      (
"You are in a third question room."
               "Question room 3"               ;101
	       )
	      (
"You are in the endgame treasure room.  A door leads out to the north, and
a hallway leads to the south."
               "Endgame treasure room"         ;102
	       )
	      (
"You are in the winner's room.  A door leads back to the south."
               "Winner's room"                 ;103
	       )
	      (
"You have reached a dead end.  There is a PC on the floor here.  Above
it is a sign that reads:
          Type the 'reset' command to type on the PC. 
A hole leads north."
               "PC area"                       ;104
               )            
))

(setq light-rooms '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 24 25 26 27 28 58 59
		     60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76
		     77 78 79 80 81 82 83))

(setq verblist '((die . die) (ne . ne) (north . n) (south . s) (east . e)
		 (west . w) (u . up) (d . down) (i . inven)
		 (inventory . inven) (look . examine) (n . n) (s . s) (e . e)
		 (w . w) (se . se) (nw . nw) (sw . sw) (up . up) 
		 (down . down) (in . in) (out . out) (go . go) (drop . drop)
		 (southeast . se) (southwest . sw) (northeast . ne)
		 (northwest . nw) (save . save-game) (restore . restore)
		 (long . long) (dig . dig) (shake . shake) (wave . shake)
		 (examine . examine) (describe . examine) (climb . climb)
		 (eat . eat) (put . dput) (type . type) (insert . dput)
		 (score . score) (help . help) (quit . quit) (read . examine)
		 (verbose . long) (urinate . piss) (piss . piss)
		 (flush . flush) (sleep . dsleep) (lie . dsleep) (x . examine)
		 (break . break) (drive . drive) (board . in) (enter . in)
		 (turn . turn) (press . press) (push . press) (swim . swim)
		 (on . in) (off . out) (chop . break) (switch . press)
		 (cut . break) (exit . out) (leave . out) (reset . dun-power)
		 (flick . press) (superb . superb) (answer . answer)
		 (throw . drop) (l . examine) (take . take) (get . take)
		 (feed . dun-feed)))

(setq inbus nil)
(setq nomail nil)
(setq ignore '(the to at))
(setq mode 'moby)
(setq sauna-level 0)

(defconst north 0)
(defconst south 1)
(defconst east 2)
(defconst west 3)
(defconst northeast 4)
(defconst southeast 5)
(defconst northwest 6)
(defconst southwest 7)
(defconst up 8)
(defconst down 9)
(defconst in 10)
(defconst out 11)

(setq dungeon-map '(
;		      no  so  ea  we  ne  se  nw  sw  up  do  in  ot
		    ( 96  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1 ) ;0
		    ( -1  -1   2  -1  -1  -1  -1  -1  -1  -1  -1  -1 ) ;1
		    ( -1  -1   3   1  -1  -1  -1  -1  -1  -1  -1  -1 ) ;2
		    ( -1  -1  -1   2   4   6  -1  -1  -1  -1  -1  -1 ) ;3
		    ( -1  -1  -1  -1   5  -1  -1   3  -1  -1  -1  -1 ) ;4
		    ( -1  -1  -1  -1  255 -1  -1   4  -1  -1  255 -1 ) ;5
		    ( -1  -1  -1  -1  -1   7   3  -1  -1  -1  -1  -1 ) ;6
		    ( -1  -1  -1  -1  -1  255  6  27  -1  -1  -1  -1 ) ;7
		    ( 255  5   9  10  -1  -1  -1   5  -1  -1  -1   5 ) ;8
		    ( -1  -1  -1   8  -1  -1  -1  -1  -1  -1  -1  -1 ) ;9
		    ( -1  -1   8  -1  -1  -1  -1  -1  -1  -1  -1  -1 ) ;10
		    ( -1   8  -1  58  -1  -1  -1  -1  -1  -1  -1  -1 ) ;11
		    ( -1  -1  13  -1  -1  -1  -1  -1  -1  -1  -1  -1 ) ;12
		    ( 15  -1  14  12  -1  -1  -1  -1  -1  -1  -1  -1 ) ;13
		    ( -1  -1  -1  13  -1  -1  -1  -1  -1  -1  -1  -1 ) ;14
		    ( -1  13  16  -1  -1  -1  -1  -1  -1  -1  -1  -1 ) ;15
		    ( -1  -1  -1  15  -1  -1  -1  -1  -1  17  16  -1 ) ;16
		    ( -1  -1  17  17  17  17 255  17 255  17  -1  -1 ) ;17
		    ( 18  18  18  18  18  -1  18  18  19  18  -1  -1 ) ;18
		    ( -1  18  18  19  19  20  19  19  -1  18  -1  -1 ) ;19
		    ( -1  -1  -1  18  -1  -1  -1  -1  -1  21  -1  -1 ) ;20
		    ( -1  -1  -1  -1  -1  20  22  -1  -1  -1  -1  -1 ) ;21
		    ( 18  18  18  18  16  18  23  18  18  18  18  18 ) ;22
		    ( -1 255  -1  -1  -1  19  -1  -1  -1  -1  -1  -1 ) ;23
		    ( 23  25  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1 ) ;24
		    ( 24 255  -1  -1  -1  -1  -1  -1  -1  -1 255  -1 ) ;25
		    (255  28  -1  -1  -1  -1  -1  -1  -1  -1 255  -1 ) ;26
		    ( -1  -1  -1  -1   7  -1  -1  -1  -1  -1  -1  -1 ) ;27
		    ( 26 255  -1  -1  -1  -1  -1  -1  -1  -1  255 -1 ) ;28
		    ( -1  -1  30  -1  -1  -1  -1  -1  -1  -1  -1  -1 ) ;29
		    ( -1  -1  31  29  -1  -1  -1  -1  -1  -1  -1  -1 ) ;30
		    ( 32  33  -1  30  -1  -1  -1  -1  -1  -1  -1  -1 ) ;31
		    ( -1  31  -1  255 -1  -1  -1  -1  -1  34  -1  -1 ) ;32
		    ( 31  -1  -1  -1  -1  -1  -1  -1  -1  35  -1  -1 ) ;33
		    ( -1  35  -1  -1  -1  -1  -1  -1  32  37  -1  -1 ) ;34
		    ( 34  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1 ) ;35
		    ( -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1 ) ;36
		    ( -1  -1  -1  -1  -1  -1  -1  38  34  -1  -1  -1 ) ;37
		    ( -1  -1  40  41  37  -1  -1  39  -1  -1  -1  -1 ) ;38
		    ( -1  -1  -1  -1  38  -1  -1  -1  -1  -1  -1  -1 ) ;39
		    ( -1  -1  -1  38  -1  -1  -1  -1  42  -1  -1  -1 ) ;40
		    ( -1  -1  38  -1  -1  -1  -1  -1  -1  43  -1  -1 ) ;41
		    ( -1  -1  -1  -1  -1  -1  -1  -1  -1  40  -1  -1 ) ;42
		    ( 44  -1  46  -1  -1  -1  -1  -1  -1  -1  -1  -1 ) ;43
		    ( -1  43  45  -1  -1  -1  -1  -1  -1  -1  -1  -1 ) ;44
		    ( -1  46  -1  44  -1  -1  -1  -1  -1  -1  -1  -1 ) ;45
		    ( 45  -1  -1  43  -1  -1  -1  -1  -1  255 -1  -1 ) ;46
		    ( 48  50  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1 ) ;47
		    ( 49  47  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1 ) ;48
		    ( -1  48  -1  -1  -1  -1  -1  -1  52  -1  -1  -1 ) ;49
		    ( 47  51  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1 ) ;50
		    ( 50  104 -1  -1  -1  -1  -1  -1  -1  -1  -1  -1 ) ;51
		    ( -1  -1  -1  -1  -1  -1  -1  -1  53  49  -1  -1 ) ;52
		    ( -1  -1  -1  -1  -1  -1  -1  -1  54  52  -1  -1 ) ;53
		    ( -1  -1  -1  -1  55  -1  -1  -1  -1  53  -1  -1 ) ;54
		    ( -1  -1  -1  -1  56  -1  -1  54  -1  -1  -1  54 ) ;55
		    ( -1  -1  -1  -1  -1  -1  -1  55  -1  31  -1  -1 ) ;56
		    ( -1  -1  32  -1  -1  -1  -1  -1  -1  -1  -1  -1 ) ;57
		    ( 59  -1  11  -1  -1  -1  -1  -1  -1  -1  255 255) ;58
		    ( 60  58  63  -1  -1  -1  255 -1  -1  -1  255 255) ;59
		    ( 61  59  64  -1  -1  -1  -1  -1  -1  -1  255 255) ;60
		    ( 62  60  65  -1  -1  -1  -1  -1  -1  -1  255 255) ;61
		    ( -1  61  66  -1  -1  -1  -1  -1  -1  -1  255 255) ;62
		    ( 64  -1  67  59  -1  -1  -1  -1  -1  -1  255 255) ;63
		    ( 65  63  68  60  -1  -1  -1  -1  -1  -1  255 255) ;64
		    ( 66  64  69  61  -1  -1  -1  -1  -1  -1  255 255) ;65
		    ( -1  65  70  62  -1  -1  -1  -1  -1  -1  255 255) ;66
		    ( 68  -1  71  63  -1  -1  -1  -1  -1  -1  255 255) ;67
		    ( 69  67  72  64  -1  -1  -1  -1  -1  -1  255 255) ;68
		    ( 70  68  73  65  -1  -1  -1  -1  -1  -1  255 255) ;69
		    ( -1  69  74  66  -1  -1  -1  -1  -1  -1  255 255) ;70
		    ( 72  -1  75  67  -1  -1  -1  -1  -1  -1  255 255) ;71
		    ( 73  71  76  68  -1  -1  -1  -1  -1  -1  255 255) ;72
		    ( 74  72  77  69  -1  -1  -1  -1  -1  -1  255 255) ;73
		    ( -1  73  78  70  -1  -1  -1  -1  -1  -1  255 255) ;74
		    ( 76  -1  79  71  -1  -1  -1  -1  -1  -1  255 255) ;75
		    ( 77  75  80  72  -1  -1  -1  -1  -1  -1  255 255) ;76
		    ( 78  76  81  73  -1  -1  -1  -1  -1  -1  255 255) ;77
		    ( -1  77  82  74  -1  -1  -1  -1  -1  -1  255 255) ;78
		    ( 80  -1  -1  75  -1  -1  -1  -1  -1  -1  255 255) ;79
		    ( 81  79  255 76  -1  -1  -1  -1  -1  -1  255 255) ;80
		    ( 82  80  -1  77  -1  -1  -1  -1  -1  -1  255 255) ;81
		    ( -1  81  -1  78  -1  -1  -1  -1  -1  -1  255 255) ;82
		    ( 84  -1  -1  -1  -1  59  -1  -1  -1  -1  255 255) ;83
		    ( -1  83  85  -1  -1  -1  -1  -1  -1  -1  -1  -1 ) ;84
		    ( 86  -1  87  84  -1  -1  -1  -1  -1  -1  -1  -1 ) ;85
		    ( -1  85  88  -1  -1  -1  -1  -1  -1  -1  -1  -1 ) ;86
		    ( 88  -1  -1  85  -1  -1  -1  -1  -1  -1  -1  -1 ) ;87
		    ( -1  87 255  86  -1  -1  -1  -1  -1  -1  -1  -1 ) ;88
		    ( -1  -1  -1  -1  -1  -1  -1  -1  -1  -1 255  -1 ) ;89
		    ( 91  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1 ) ;90
		    ( 92  90  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1 ) ;91
		    ( -1  91  -1  -1  -1  -1  -1  -1  93  94  -1  -1 ) ;92
		    ( -1  -1  -1  88  -1  -1  -1  -1  -1  92  -1  -1 ) ;93
		    ( -1  -1  -1  -1  95  -1  -1  -1  92  -1  -1  -1 ) ;94
		    ( -1  -1  -1  -1  -1  -1  -1  94  -1  -1  -1  -1 ) ;95
		    ( 97   0  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1 ) ;96
		    ( -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1 ) ;97
		    ( 99  97  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1 ) ;98
		    ( -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1 ) ;99
		    ( 101 99  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1 ) ;100
		    ( -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1 ) ;101
		    ( 103 101 -1  -1  -1  -1  -1  -1  -1  -1  -1  -1 ) ;102
		    ( -1  102 -1  -1  -1  -1  -1  -1  -1  -1  -1  -1 ) ;103
		    ( 51  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1 ) ;104
		    )
;		      no  so  ea  we  ne  se  nw  sw  up  do  in  ot
)


;;; How the user references *all* objects, permanent and regular.
(setq objnames '(
		 (shovel . 0) 
		 (lamp . 1)
		 (cpu . 2) (board . 2) (card . 2)
		 (food . 3) 
		 (key . 4) 
		 (paper . 5)
		 (rms . 6) (statue . 6) (statuette . 6)  (stallman . 6)
		 (diamond . 7)
		 (weight . 8)
		 (life . 9) (preserver . 9)
		 (bracelet . 10) (emerald . 10) 
		 (gold . 11)
		 (platinum . 12)
		 (towel . 13) (beach . 13)
		 (axe . 14)
		 (silver . 15)
		 (license . 16)
		 (coins . 17)
		 (egg . 18)
		 (jar . 19)
		 (bone . 20)
		 (acid . 21) (nitric . 21)
		 (glycerine . 22)
		 (ruby . 23)
		 (amethyst . 24) 
		 (mona . 25)
		 (bill . 26) 
		 (floppy . 27) (disk . 27)
		 
		 (boulder . -1)
		 (tree . -2) (trees . -2) 
		 (bear . -3)
		 (bin . -4) (bins . -4)
		 (cabinet . -5) (computer . -5) (vax . -5) (ibm . -5) 
		 (protoplasm . -6)
		 (dial . -7) 
		 (button . -8) 
		 (chute . -9) 
		 (painting . -10)
		 (bed . -11)
		 (urinal . -12)
		 (URINE . -13)
		 (pipes . -14) (pipe . -14) 
		 (box . -15) (slit . -15) 
		 (cable . -16) (ethernet . -16) 
		 (mail . -17) (drop . -17)
		 (bus . -18)
		 (gate . -19)
		 (cliff . -20) 
		 (skeleton . -21) (dinosaur . -21)
		 (fish . -22)
		 (tanks . -23)
		 (switch . -24)
		 (blackboard . -25)
		 (disposal . -26) (garbage . -26)
		 (ladder . -27)
		 (subway . -28) (train . -28) 
		 (pc . -29) (drive . -29)
))

(dolist (x objnames)
  (let (name)
    (setq name (concat "obj-" (prin1-to-string (car x))))
    (eval (list 'defconst (intern name) (cdr x)))))

(defconst obj-special 255)

;;; The initial setup of what objects are in each room.
;;; Regular objects have whole numbers lower than 255.
;;; Objects that cannot be taken but might move and are
;;; described during room description are negative.
;;; Stuff that is described and might change are 255, and are
;;; handled specially by 'describe-room. 

(setq room-objects (list nil 

        (list obj-shovel)                     ;; treasure-room
        (list obj-boulder)                    ;; dead-end
        nil nil nil
        (list obj-food)                       ;; se-nw-road
        (list obj-bear)                       ;; bear-hangout
        nil nil
        (list obj-special)                    ;; computer-room
        (list obj-lamp obj-license obj-silver);; meadow
        nil nil
        (list obj-special)                    ;; sauna
        nil 
        (list obj-weight obj-life)            ;; weight-room
        nil nil
        (list obj-rms obj-floppy)             ;; thirsty-maze
        nil nil nil nil nil nil nil 
        (list obj-emerald)                    ;; hidden-area
        nil
        (list obj-gold)                       ;; misty-room
        nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
        (list obj-towel obj-special)          ;; red-room
        nil nil nil nil nil
        (list obj-box)                        ;; stair-landing
        nil nil nil
        (list obj-axe)                        ;; smal-crawlspace
        nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
        nil nil nil nil nil
        (list obj-special)                    ;; fourth-vermont-intersection
        nil nil
        (list obj-coins)                      ;; fifth-oaktree-intersection
        nil
        (list obj-bus)                        ;; fifth-sycamore-intersection
        nil
        (list obj-bone)                       ;; museum-lobby
        nil
        (list obj-jar obj-special obj-ruby)   ;; marine-life-area
        (list obj-nitric)                     ;; maintenance-room
        (list obj-glycerine)                  ;; classroom
        nil nil nil nil nil
        (list obj-amethyst)                   ;; bottom-of-subway-stairs
        nil nil
        (list obj-special)                    ;; question-room-1
        nil
        (list obj-special)                    ;; question-room-2
        nil
        (list obj-special)                    ;; question-room-three
        nil
        (list obj-mona)                       ;; winner's-room
nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
nil))

;;; These are objects in a room that are only described in the
;;; room description.  They are permanent.

(setq room-silents (list nil
        (list obj-tree)                        ;; dead-end
        (list obj-tree)                        ;; e-w-dirt-road
        nil nil nil nil nil nil
        (list obj-bin)                         ;; mailroom
        (list obj-computer)                    ;; computer-room
        nil nil nil
        (list obj-dial)                        ;; sauna
        nil
        (list obj-ladder)                      ;; weight-room
        (list obj-button obj-ladder)           ;; maze-button-room
        nil nil nil
        nil nil nil nil nil nil nil
        (list obj-chute)                       ;; cave-entrance
        nil nil nil nil nil
        (list obj-painting obj-bed)            ;; bedroom
        (list obj-urinal obj-pipes)            ;; bathroom
        nil nil nil nil nil nil
        (list obj-boulder)                     ;; horseshoe-boulder-room
        nil nil nil nil nil nil nil nil nil nil nil nil nil nil
        (list obj-computer obj-cable)          ;; gamma-computing-center
        (list obj-mail)                        ;; post-office
        (list obj-gate)                        ;; main-maple-intersection
        nil nil nil nil nil nil nil nil nil nil nil nil nil
        nil nil nil nil nil nil nil
        (list obj-cliff)                       ;; fifth-oaktree-intersection
        nil nil nil
        (list obj-dinosaur)                    ;; museum-lobby
        nil
        (list obj-fish obj-tanks)              ;; marine-life-area
        (list obj-switch)                      ;; maintenance-room
        (list obj-blackboard)                  ;; classroom
        (list obj-train)                       ;; vermont-station
        nil nil
        (list obj-disposal)                    ;; north-end-of-n-s-tunnel
        nil nil
        (list obj-computer)                    ;; endgame-computer-room
        nil nil nil nil nil nil nil nil 
	(list obj-pc)                          ;; pc-area
	nil nil nil nil nil nil
))
(setq inventory '(1))

;; Descriptions of objects, as they appear in the room description, and
;; the inventory.

(setq objects '(
		("There is a shovel here." "A shovel")                ;0
		("There is a lamp nearby." "A lamp")                  ;1
		("There is a CPU card here." "A computer board")      ;2
		("There is some food here." "Some food")              ;3
		("There is a shiny brass key here." "A brass key")    ;4
		("There is a slip of paper here." "A slip of paper")  ;5
		("There is a wax statuette of Richard Stallman here." ;6
		 "An RMS statuette")
		("There is a shimmering diamond here." "A diamond")   ;7
		("There is a 10 pound weight here." "A weight")       ;8
		("There is a life preserver here." "A life preserver");9
		("There is an emerald bracelet here." "A bracelet")   ;10
		("There is a gold bar here." "A gold bar")            ;11
		("There is a platinum bar here." "A platinum bar")    ;12
		("There is a beach towel on the ground here." "A beach towel")
		("There is an axe here." "An axe") ;14
		("There is a silver bar here." "A silver bar")  ;15
		("There is a bus driver's license here." "A license") ;16
		("There are some valuable coins here." "Some valuable coins")
		("There is a jewel-encrusted egg here." "A valuable egg") ;18
		("There is a glass jar here." "A glass jar") ;19
		("There is a dinosaur bone here." "A bone") ;20
		("There is a packet of nitric acid here." "Some nitric acid")
		("There is a packet of glycerine here." "Some glycerine") ;22
		("There is a valuable ruby here." "A ruby") ;23
		("There is a valuable amethyst here." "An amethyst") ;24
		("The Mona Lisa is here." "The Mona Lisa") ;25
		("There is a 100 dollar bill here." "A $100 bill") ;26
		("There is a floppy disk here." "A floppy disk") ;27
	       )
)

;;; Weight of objects

(setq object-lbs '(2 1 1 1 1 0 2 2 10 3 1 1 1 0 1 1 0 1 1 1 1 0 0 2 2 1 0 0))
(setq object-pts '(0 0 0 0 0 0 0 10 0 0 10 10 10 0 0 10 0 10 10 0 0 0 0 10 10 10 10 0))


;;; Unix representation of objects.
(setq objfiles '(
		 "shovel.o" "lamp.o" "cpu.o" "food.o" "key.o" "paper.o"
		 "rms.o" "diamond.o" "weight.o" "preserver.o" "bracelet.o"
		 "gold.o" "platinum.o" "towel.o" "axe.o" "silver.o" "license.o"
		 "coins.o" "egg.o" "jar.o" "bone.o" "nitric.o" "glycerine.o"
		 "ruby.o" "amethyst.o"
		 ))

;;; These are the descriptions for the negative numbered objects from
;;; room-objects

(setq perm-objects '(
		     nil
		     ("There is a large boulder here.")
		     nil
		     ("There is a ferocious bear here!")
		     nil
		     nil
		     ("There is a worthless pile of protoplasm here.")
		     nil
		     nil
		     nil
		     nil
		     nil
		     nil
		     ("There is a strange smell in this room.")
		     nil
		     (
"There is a box with a slit in it, bolted to the wall here."
                     )
		     nil
		     nil
		     ("There is a bus here.")
		     nil
		     nil
		     nil
))


;;; These are the descriptions the user gets when regular objects are
;;; examined.

(setq physobj-desc '(
"It is a normal shovel with a price tag attached that says $19.99."
"The lamp is hand-crafted by Geppetto."
"The CPU board has a VAX chip on it.  It seems to have
2 Megabytes of RAM onboard."
"It looks like some kind of meat.  Smells pretty bad."
nil
"The paper says: Don't forget to type 'help' for help.  Also, remember
this word: 'worms'"
"The statuette is of the likeness of Richard Stallman, the author of the
famous EMACS editor.  You notice that he is not wearing any shoes."
nil
"You observe that the weight is heavy."
"It says S. S. Minnow."
nil
nil
nil
"It has a picture of snoopy on it."
nil
nil
"It has your picture on it!"
"They are old coins from the 19th century."
"It is a valuable Fabrege egg."
"It is a a plain glass jar."
nil
nil
nil
nil
nil
                     )
)

;;; These are the descriptions the user gets when non-regular objects
;;; are examined.

(setq permobj-desc '(
		     nil
"It is just a boulder.  It cannot be moved."
"They are palm trees with a bountiful supply of coconuts in them."
"It looks like a grizzly to me."
"All of the bins are empty.  Looking closely you can see that there
are names written at the bottom of each bin, but most of them are
faded away so that you cannot read them.  You can only make out three
names:
                   Jeffrey Collier
                   Robert Toukmond
                   Thomas Stock
"
                      nil
"It is just a garbled mess."
"The dial points to a temperature scale which has long since faded away."
nil
nil
"It is a velvet painting of Elvis Presly.  It seems to be nailed to the
wall, and you cannot move it."
"It is a queen sized bed, with a very firm mattress."
"The urinal is very clean compared with everything else in the cave.  There
isn't even any rust.  Upon close examination you realize that the drain at the
bottom is missing, and there is just a large hole leading down the
pipes into nowhere.  The hole is too small for a person to fit in.  The 
flush handle is so clean that you can see your reflection in it."
nil
nil
"The box has a slit in the top of it, and on it, in sloppy handwriting, is
written: 'For key upgrade, put key in here.'"
nil
"It says 'express mail' on it."
"It is a 35 passenger bus with the company name 'mobytours' on it."
"It is a large metal gate that is too big to climb over."
"It is a HIGH cliff."
"Unfortunately you do not know enough about dinosaurs to tell very much about
it.  It is very big, though."
"The fish look like they were once quite beautiful."
nil
nil
nil
nil
"It is a normal ladder that is permanently attached to the hole."
"It is a passenger train that is ready to go."
"It is a personal computer that has only one floppy disk drive."
		    )
)

(setq diggables (list nil nil nil (list obj-cpu) nil nil nil nil nil nil nil
		  nil nil nil nil nil nil nil nil nil nil      ;11-20
		  nil nil nil nil nil nil nil nil nil nil      ;21-30
		  nil nil nil nil nil nil nil nil nil nil      ;31-40
		  nil (list obj-platinum) nil nil nil nil nil nil nil nil))

(setq scroll-step 2)
(setq room-shorts nil)
(dolist (x rooms)
  (setq room-shorts  
		     (append room-shorts (list (downcase (space-to-hyphen
							  (cadr x)))))))

(setq endgame-questions '(
			  (
"What is your password on the machine called 'pokey'?" "robert")
			  (
"What password did you use during anonymous ftp to gamma?" "foo")
			  (
"Excluding the endgame, how many places are there where you can put
treasures for points?" "4" "four")
			  (
"What is your login name on the 'endgame' machine?" "toukmond"
)
			  (
"What is the nearest whole dollar to the price of the shovel?" "20" "twenty")
			  (
"What is the name of the bus company serving the town?" "mobytours")
			  (
"Give either of the two last names in the mailroom, other than your own."
"collier" "stock")
			  (
"What cartoon character is on the towel?" "snoopy")
			  (
"What is the last name of the author of EMACS?" "stallman")
			  (
"How many megabytes of memory is on the CPU board for the Vax?" "2")
			  (
"Which street in town is named after a U.S. state?" "vermont")
			  (
"How many pounds did the weight weigh?" "ten" "10")
			  (
"Name the STREET which runs right over the subway stop." "fourth" "4" "4th")
			  (
"How many corners are there in town (excluding the one with the Post Office)?"
                  "24" "twentyfour" "twenty-four")
			  (
"What type of bear was hiding your key?" "grizzly")
			  (
"Name either of the two objects you found by digging." "cpu" "card" "vax"
"board" "platinum")
			  (
"What network protocol is used between pokey and gamma?" "tcp/ip" "ip" "tcp")
))

(let (a)
  (setq a 0)
  (dolist (x room-shorts)
    (eval (list 'defconst (intern x) a))
    (setq a (+ a 1))))


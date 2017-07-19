;; A simple game to test all the bits work
;; 3 rooms
;; EXAMINE WALL
;; TAKE COIN
;; OPEN DOOR
;; NORTH
;; TAKE SPANNER
;; TALK GOBLIN
;; TALK GOBLIN
;; TALK GOBLIN
;; GIVE COIN GOBLIN or GIVE GOBLIN COIN
;; EXAMINE SCROLL
;; SAY FRANGIBLE
;; ENTER PORTAL
;; CAST SPELL
;; THROW SPANNER WIZARD

;; TODO make a file called 'declarations' or somesuch, with
;; defloc, defresponse etc


(dloc :dungeon-cell "DUNGEON CELL" "/home/dan/Downloads/cellardoor.bmp" :right
      "You are in the dungeon prison of Wangband underneath the fortress of the Black Wizard, Beelzepops. Home to stench-rats, were-toads, sniveling goblins and you. Of the current denizens, you are currently the most wretched. Surrounding you is a slime that reminds you of lime pudding, if lime pudding had hundreds of eyes and made a rasping, wheezing sound as it oozed out of the cracks in the wall. You must escape.")


(with-location :dungeon-cell
  (response '(EXAMINE WALL) "Millions of sad eyes gaze at you from the wall slime")
  
  

;;A goblin appears at the door. He flings some inedible slop through the bars.

(dloc :frazbolgs-closet "FRAZBOLG'S CLOSET" "/home/dan/Downloads/goblincloset.bmp" :right
      "You are in the well-appointed closet of the goblin guard Frazbolg. Over centuries of guarding his prisoners he has amassed an impressive collection of posessions, a single chair (broken), a spare loin-cloth (tattered) and a hundred-year-old copy of Modern Necromancer magazine.")

;;You see a spanner

;;Frazbolg is engrossed in an article 'Entrails. Worth the mess?'
;;Frazbolg is engrossed in an article 'Platelet infusions. Here's why.'
;;Frazbolg is engrossed in an article 'Tealeaves. Primitive superstition or refreshing alternative to intestinal tracts?'
;;Frazbolg is engrossed in an article 'Cardio for Graverobbing.'
;;Frazbolg is engrossed in an article 'Lady necromancers. Here to stay?'
;;Frazbolg is engrossed in an article 'One weird trick for a bloated corpse.'
;;Frazbolg says 'I am stuck here until I can afford Necromancer school.'



(dloc :wizards-tower "WIZARD'S TOWER" "/home/dan/Downloads/wizardstower.bmp" :right
      "You are in the tower of the Black Wizard, Beelzepops. 

/*
DOOM generic portado para Harbour com interfaces minimas em C para acesso a allegro.

Por Wagner Nunes da Silva

vagucs@bol.com.br
vagucs@vagucs.com.br
vagucs@gmail.com

www.vagucs.com.br
*/
#ifndef __D_ENGLSH__
#define __D_ENGLSH__

#define D_DEVSTR e"Development mode ON.\n"
#define D_CDROM e"CD-ROM Version: default.cfg from c:\\doomdata\n"

#define PRESSKEY e"press a key."
#define PRESSYN e"press y or n."
#define QUITMSG e"are you sure you want to\nquit this great game?"
#define LOADNET e"you can't do load while in a net game!\n\n" + PRESSKEY
#define QLOADNET e"you can't quickload during a netgame!\n\n" + PRESSKEY
#define QSAVESPOT e"you haven't picked a quicksave slot yet!\n\n" + PRESSKEY
#define SAVEDEAD e"you can't save if you aren't playing!\n\n" + PRESSKEY
#define QSPROMPT e"quicksave over your game named\n\n'%s'?\n\n" + PRESSYN
#define QLPROMPT e"do you want to quickload the game named\n\n'%s'?\n\n" + PRESSYN

#define NEWGAME e"you can't start a new game\n" + e"while in a network game.\n\n" + PRESSKEY

#define NIGHTMARE e"are you sure? this skill level\n" + e"isn't even remotely fair.\n\n" + PRESSYN

#define SWSTRING e"this is the shareware version of doom.\n\n" + ;
    e"you need to order the entire trilogy.\n\n" + ;
    PRESSKEY

#define MSGOFF e"Messages OFF"
#define MSGON e"Messages ON"
#define NETEND e"you can't end a netgame!\n\n" + PRESSKEY
#define ENDGAME e"are you sure you want to end the game?\n\n" + PRESSYN

#define DOSY e"(press y to quit to dos.)"

#define DETAILHI e"High detail"
#define DETAILLO e"Low detail"
#define GAMMALVL0 e"Gamma correction OFF"
#define GAMMALVL1 e"Gamma correction level 1"
#define GAMMALVL2 e"Gamma correction level 2"
#define GAMMALVL3 e"Gamma correction level 3"
#define GAMMALVL4 e"Gamma correction level 4"
#define EMPTYSTRING e"empty slot"

#define GOTARMOR e"Picked up the armor."
#define GOTMEGA e"Picked up the MegaArmor!"
#define GOTHTHBONUS e"Picked up a health bonus."
#define GOTARMBONUS e"Picked up an armor bonus."
#define GOTSTIM e"Picked up a stimpack."
#define GOTMEDINEED e"Picked up a medikit that you REALLY need!"
#define GOTMEDIKIT e"Picked up a medikit."
#define GOTSUPER e"Supercharge!"

#define GOTBLUECARD e"Picked up a blue keycard."
#define GOTYELWCARD e"Picked up a yellow keycard."
#define GOTREDCARD e"Picked up a red keycard."
#define GOTBLUESKUL e"Picked up a blue skull key."
#define GOTYELWSKUL e"Picked up a yellow skull key."
#define GOTREDSKULL e"Picked up a red skull key."

#define GOTINVUL e"Invulnerability!"
#define GOTBERSERK e"Berserk!"
#define GOTINVIS e"Partial Invisibility"
#define GOTSUIT e"Radiation Shielding Suit"
#define GOTMAP e"Computer Area Map"
#define GOTVISOR e"Light Amplification Visor"
#define GOTMSPHERE e"MegaSphere!"

#define GOTCLIP e"Picked up a clip."
#define GOTCLIPBOX e"Picked up a box of bullets."
#define GOTROCKET e"Picked up a rocket."
#define GOTROCKBOX e"Picked up a box of rockets."
#define GOTCELL e"Picked up an energy cell."
#define GOTCELLBOX e"Picked up an energy cell pack."
#define GOTSHELLS e"Picked up 4 shotgun shells."
#define GOTSHELLBOX e"Picked up a box of shotgun shells."
#define GOTBACKPACK e"Picked up a backpack full of ammo!"

#define GOTBFG9000 e"You got the BFG9000!  Oh, yes."
#define GOTCHAINGUN e"You got the chaingun!"
#define GOTCHAINSAW e"A chainsaw!  Find some meat!"
#define GOTLAUNCHER e"You got the rocket launcher!"
#define GOTPLASMA e"You got the plasma gun!"
#define GOTSHOTGUN e"You got the shotgun!"
#define GOTSHOTGUN2 e"You got the super shotgun!"

#define PD_BLUEO e"You need a blue key to activate this object"
#define PD_REDO e"You need a red key to activate this object"
#define PD_YELLOWO e"You need a yellow key to activate this object"
#define PD_BLUEK e"You need a blue key to open this door"
#define PD_REDK e"You need a red key to open this door"
#define PD_YELLOWK e"You need a yellow key to open this door"

#define GGSAVED e"game saved."

#define HUSTR_MSGU e"[Message unsent]"

#define HUSTR_E1M1 e"E1M1: Hangar"
#define HUSTR_E1M2 e"E1M2: Nuclear Plant"
#define HUSTR_E1M3 e"E1M3: Toxin Refinery"
#define HUSTR_E1M4 e"E1M4: Command Control"
#define HUSTR_E1M5 e"E1M5: Phobos Lab"
#define HUSTR_E1M6 e"E1M6: Central Processing"
#define HUSTR_E1M7 e"E1M7: Computer Station"
#define HUSTR_E1M8 e"E1M8: Phobos Anomaly"
#define HUSTR_E1M9 e"E1M9: Military Base"

#define HUSTR_E2M1 e"E2M1: Deimos Anomaly"
#define HUSTR_E2M2 e"E2M2: Containment Area"
#define HUSTR_E2M3 e"E2M3: Refinery"
#define HUSTR_E2M4 e"E2M4: Deimos Lab"
#define HUSTR_E2M5 e"E2M5: Command Center"
#define HUSTR_E2M6 e"E2M6: Halls of the Damned"
#define HUSTR_E2M7 e"E2M7: Spawning Vats"
#define HUSTR_E2M8 e"E2M8: Tower of Babel"
#define HUSTR_E2M9 e"E2M9: Fortress of Mystery"

#define HUSTR_E3M1 e"E3M1: Hell Keep"
#define HUSTR_E3M2 e"E3M2: Slough of Despair"
#define HUSTR_E3M3 e"E3M3: Pandemonium"
#define HUSTR_E3M4 e"E3M4: House of Pain"
#define HUSTR_E3M5 e"E3M5: Unholy Cathedral"
#define HUSTR_E3M6 e"E3M6: Mt. Erebus"
#define HUSTR_E3M7 e"E3M7: Limbo"
#define HUSTR_E3M8 e"E3M8: Dis"
#define HUSTR_E3M9 e"E3M9: Warrens"

#define HUSTR_E4M1 e"E4M1: Hell Beneath"
#define HUSTR_E4M2 e"E4M2: Perfect Hatred"
#define HUSTR_E4M3 e"E4M3: Sever The Wicked"
#define HUSTR_E4M4 e"E4M4: Unruly Evil"
#define HUSTR_E4M5 e"E4M5: They Will Repent"
#define HUSTR_E4M6 e"E4M6: Against Thee Wickedly"
#define HUSTR_E4M7 e"E4M7: And Hell Followed"
#define HUSTR_E4M8 e"E4M8: Unto The Cruel"
#define HUSTR_E4M9 e"E4M9: Fear"

#define HUSTR_1 e"level 1: entryway"
#define HUSTR_2 e"level 2: underhalls"
#define HUSTR_3 e"level 3: the gantlet"
#define HUSTR_4 e"level 4: the focus"
#define HUSTR_5 e"level 5: the waste tunnels"
#define HUSTR_6 e"level 6: the crusher"
#define HUSTR_7 e"level 7: dead simple"
#define HUSTR_8 e"level 8: tricks and traps"
#define HUSTR_9 e"level 9: the pit"
#define HUSTR_10 e"level 10: refueling base"
#define HUSTR_11 e"level 11: 'o' of destruction!"

#define HUSTR_12 e"level 12: the factory"
#define HUSTR_13 e"level 13: downtown"
#define HUSTR_14 e"level 14: the inmost dens"
#define HUSTR_15 e"level 15: industrial zone"
#define HUSTR_16 e"level 16: suburbs"
#define HUSTR_17 e"level 17: tenements"
#define HUSTR_18 e"level 18: the courtyard"
#define HUSTR_19 e"level 19: the citadel"
#define HUSTR_20 e"level 20: gotcha!"

#define HUSTR_21 e"level 21: nirvana"
#define HUSTR_22 e"level 22: the catacombs"
#define HUSTR_23 e"level 23: barrels o' fun"
#define HUSTR_24 e"level 24: the chasm"
#define HUSTR_25 e"level 25: bloodfalls"
#define HUSTR_26 e"level 26: the abandoned mines"
#define HUSTR_27 e"level 27: monster condo"
#define HUSTR_28 e"level 28: the spirit world"
#define HUSTR_29 e"level 29: the living end"
#define HUSTR_30 e"level 30: icon of sin"

#define HUSTR_31 e"level 31: wolfenstein"
#define HUSTR_32 e"level 32: grosse"

#define PHUSTR_1 e"level 1: congo"
#define PHUSTR_2 e"level 2: well of souls"
#define PHUSTR_3 e"level 3: aztec"
#define PHUSTR_4 e"level 4: caged"
#define PHUSTR_5 e"level 5: ghost town"
#define PHUSTR_6 e"level 6: baron's lair"
#define PHUSTR_7 e"level 7: caughtyard"
#define PHUSTR_8 e"level 8: realm"
#define PHUSTR_9 e"level 9: abattoire"
#define PHUSTR_10 e"level 10: onslaught"
#define PHUSTR_11 e"level 11: hunted"

#define PHUSTR_12 e"level 12: speed"
#define PHUSTR_13 e"level 13: the crypt"
#define PHUSTR_14 e"level 14: genesis"
#define PHUSTR_15 e"level 15: the twilight"
#define PHUSTR_16 e"level 16: the omen"
#define PHUSTR_17 e"level 17: compound"
#define PHUSTR_18 e"level 18: neurosphere"
#define PHUSTR_19 e"level 19: nme"
#define PHUSTR_20 e"level 20: the death domain"

#define PHUSTR_21 e"level 21: slayer"
#define PHUSTR_22 e"level 22: impossible mission"
#define PHUSTR_23 e"level 23: tombstone"
#define PHUSTR_24 e"level 24: the final frontier"
#define PHUSTR_25 e"level 25: the temple of darkness"
#define PHUSTR_26 e"level 26: bunker"
#define PHUSTR_27 e"level 27: anti-christ"
#define PHUSTR_28 e"level 28: the sewers"
#define PHUSTR_29 e"level 29: odyssey of noises"
#define PHUSTR_30 e"level 30: the gateway of hell"

#define PHUSTR_31 e"level 31: cyberden"
#define PHUSTR_32 e"level 32: go 2 it"

#define THUSTR_1 e"level 1: system control"
#define THUSTR_2 e"level 2: human bbq"
#define THUSTR_3 e"level 3: power control"
#define THUSTR_4 e"level 4: wormhole"
#define THUSTR_5 e"level 5: hanger"
#define THUSTR_6 e"level 6: open season"
#define THUSTR_7 e"level 7: prison"
#define THUSTR_8 e"level 8: metal"
#define THUSTR_9 e"level 9: stronghold"
#define THUSTR_10 e"level 10: redemption"
#define THUSTR_11 e"level 11: storage facility"

#define THUSTR_12 e"level 12: crater"
#define THUSTR_13 e"level 13: nukage processing"
#define THUSTR_14 e"level 14: steel works"
#define THUSTR_15 e"level 15: dead zone"
#define THUSTR_16 e"level 16: deepest reaches"
#define THUSTR_17 e"level 17: processing area"
#define THUSTR_18 e"level 18: mill"
#define THUSTR_19 e"level 19: shipping/respawning"
#define THUSTR_20 e"level 20: central processing"

#define THUSTR_21 e"level 21: administration center"
#define THUSTR_22 e"level 22: habitat"
#define THUSTR_23 e"level 23: lunar mining project"
#define THUSTR_24 e"level 24: quarry"
#define THUSTR_25 e"level 25: baron's den"
#define THUSTR_26 e"level 26: ballistyx"
#define THUSTR_27 e"level 27: mount pain"
#define THUSTR_28 e"level 28: heck"
#define THUSTR_29 e"level 29: river styx"
#define THUSTR_30 e"level 30: last call"

#define THUSTR_31 e"level 31: pharaoh"
#define THUSTR_32 e"level 32: caribbean"

#define HUSTR_CHATMACRO1 e"I'm ready to kick butt!"
#define HUSTR_CHATMACRO2 e"I'm OK."
#define HUSTR_CHATMACRO3 e"I'm not looking too good!"
#define HUSTR_CHATMACRO4 e"Help!"
#define HUSTR_CHATMACRO5 e"You suck!"
#define HUSTR_CHATMACRO6 e"Next time, scumbag..."
#define HUSTR_CHATMACRO7 e"Come here!"
#define HUSTR_CHATMACRO8 e"I'll take care of it."
#define HUSTR_CHATMACRO9 e"Yes"
#define HUSTR_CHATMACRO0 e"No"

#define HUSTR_TALKTOSELF1 e"You mumble to yourself"
#define HUSTR_TALKTOSELF2 e"Who's there?"
#define HUSTR_TALKTOSELF3 e"You scare yourself"
#define HUSTR_TALKTOSELF4 e"You start to rave"
#define HUSTR_TALKTOSELF5 e"You've lost it..."

#define HUSTR_MESSAGESENT e"[Message Sent]"

#define HUSTR_PLRGREEN e"Green: "
#define HUSTR_PLRINDIGO e"Indigo: "
#define HUSTR_PLRBROWN e"Brown: "
#define HUSTR_PLRRED e"Red: "

#define HUSTR_KEYGREEN ( Asc( "g" ) & 0xFF )
#define HUSTR_KEYINDIGO ( Asc( "i" ) & 0xFF )
#define HUSTR_KEYBROWN ( Asc( "b" ) & 0xFF )
#define HUSTR_KEYRED ( Asc( "r" ) & 0xFF )

#define AMSTR_FOLLOWON e"Follow Mode ON"
#define AMSTR_FOLLOWOFF e"Follow Mode OFF"

#define AMSTR_GRIDON e"Grid ON"
#define AMSTR_GRIDOFF e"Grid OFF"

#define AMSTR_MARKEDSPOT e"Marked Spot"
#define AMSTR_MARKSCLEARED e"All Marks Cleared"

#define STSTR_MUS e"Music Change"
#define STSTR_NOMUS e"IMPOSSIBLE SELECTION"
#define STSTR_DQDON e"Degreelessness Mode On"
#define STSTR_DQDOFF e"Degreelessness Mode Off"

#define STSTR_KFAADDED e"Very Happy Ammo Added"
#define STSTR_FAADDED e"Ammo (no keys) Added"

#define STSTR_NCON e"No Clipping Mode ON"
#define STSTR_NCOFF e"No Clipping Mode OFF"

#define STSTR_BEHOLD e"inVuln, Str, Inviso, Rad, Allmap, or Lite-amp"
#define STSTR_BEHOLDX e"Power-up Toggled"

#define STSTR_CHOPPERS e"... doesn't suck - GM"
#define STSTR_CLEV e"Changing Level..."

#define E1TEXT e"Once you beat the big badasses and\n" + ;
    e"clean out the moon base you're supposed\n" + ;
    e"to win, aren't you? Aren't you? Where's\n" + ;
    e"your fat reward and ticket home? What\n" + ;
    e"the hell is this? It's not supposed to\n" + ;
    e"end this way!\n" + ;
    e"\n" + ;
    e"It stinks like rotten meat, but looks\n" + ;
    e"like the lost Deimos base.  Looks like\n" + ;
    e"you're stuck on The Shores of Hell.\n" + ;
    e"The only way out is through.\n" + ;
    e"\n" + ;
    e"To continue the DOOM experience, play\n" + ;
    e"The Shores of Hell and its amazing\n" + ;
    e"sequel, Inferno!\n"

#define E2TEXT e"You've done it! The hideous cyber-\n" + ;
    e"demon lord that ruled the lost Deimos\n" + ;
    e"moon base has been slain and you\n" + ;
    e"are triumphant! But ... where are\n" + ;
    e"you? You clamber to the edge of the\n" + ;
    e"moon and look down to see the awful\n" + ;
    e"truth.\n" + ;
    e"\n" + ;
    e"Deimos floats above Hell itself!\n" + ;
    e"You've never heard of anyone escaping\n" + ;
    e"from Hell, but you'll make the bastards\n" + ;
    e"sorry they ever heard of you! Quickly,\n" + ;
    e"you rappel down to  the surface of\n" + ;
    e"Hell.\n" + ;
    e"\n" + ;
    e"Now, it's on to the final chapter of\n" + ;
    e"DOOM! -- Inferno."

#define E3TEXT e"The loathsome spiderdemon that\n" + ;
    e"masterminded the invasion of the moon\n" + ;
    e"bases and caused so much death has had\n" + ;
    e"its ass kicked for all time.\n" + ;
    e"\n" + ;
    e"A hidden doorway opens and you enter.\n" + ;
    e"You've proven too tough for Hell to\n" + ;
    e"contain, and now Hell at last plays\n" + ;
    e"fair -- for you emerge from the door\n" + ;
    e"to see the green fields of Earth!\n" + ;
    e"Home at last.\n" + ;
    e"\n" + ;
    e"You wonder what's been happening on\n" + ;
    e"Earth while you were battling evil\n" + ;
    e"unleashed. It's good that no Hell-\n" + ;
    e"spawn could have come through that\n" + ;
    e"door with you ..."

#define E4TEXT e"the spider mastermind must have sent forth\n" + ;
    e"its legions of hellspawn before your\n" + ;
    e"final confrontation with that terrible\n" + ;
    e"beast from hell.  but you stepped forward\n" + ;
    e"and brought forth eternal damnation and\n" + ;
    e"suffering upon the horde as a true hero\n" + ;
    e"would in the face of something so evil.\n" + ;
    e"\n" + ;
    e"besides, someone was gonna pay for what\n" + ;
    e"happened to daisy, your pet rabbit.\n" + ;
    e"\n" + ;
    e"but now, you see spread before you more\n" + ;
    e"potential pain and gibbitude as a nation\n" + ;
    e"of demons run amok among our cities.\n" + ;
    e"\n" + ;
    e"next stop, hell on earth!"

#define C1TEXT e"YOU HAVE ENTERED DEEPLY INTO THE INFESTED\n" + ;
    e"STARPORT. BUT SOMETHING IS WRONG. THE\n" + ;
    e"MONSTERS HAVE BROUGHT THEIR OWN REALITY\n" + ;
    e"WITH THEM, AND THE STARPORT'S TECHNOLOGY\n" + ;
    e"IS BEING SUBVERTED BY THEIR PRESENCE.\n" + ;
    e"\n" + ;
    e"AHEAD, YOU SEE AN OUTPOST OF HELL, A\n" + ;
    e"FORTIFIED ZONE. IF YOU CAN GET PAST IT,\n" + ;
    e"YOU CAN PENETRATE INTO THE HAUNTED HEART\n" + ;
    e"OF THE STARBASE AND FIND THE CONTROLLING\n" + ;
    e"SWITCH WHICH HOLDS EARTH'S POPULATION\n" + ;
    e"HOSTAGE."

#define C2TEXT e"YOU HAVE WON! YOUR VICTORY HAS ENABLED\n" + ;
    e"HUMANKIND TO EVACUATE EARTH AND ESCAPE\n" + ;
    e"THE NIGHTMARE.  NOW YOU ARE THE ONLY\n" + ;
    e"HUMAN LEFT ON THE FACE OF THE PLANET.\n" + ;
    e"CANNIBAL MUTATIONS, CARNIVOROUS ALIENS,\n" + ;
    e"AND EVIL SPIRITS ARE YOUR ONLY NEIGHBORS.\n" + ;
    e"YOU SIT BACK AND WAIT FOR DEATH, CONTENT\n" + ;
    e"THAT YOU HAVE SAVED YOUR SPECIES.\n" + ;
    e"\n" + ;
    e"BUT THEN, EARTH CONTROL BEAMS DOWN A\n" + ;
    e"MESSAGE FROM SPACE: \"SENSORS HAVE LOCATED\n" + ;
    e"THE SOURCE OF THE ALIEN INVASION. IF YOU\n" + ;
    e"GO THERE, YOU MAY BE ABLE TO BLOCK THEIR\n" + ;
    e"ENTRY.  THE ALIEN BASE IS IN THE HEART OF\n" + ;
    e"YOUR OWN HOME CITY, NOT FAR FROM THE\n" + ;
    e"STARPORT.\" SLOWLY AND PAINFULLY YOU GET\n" + ;
    e"UP AND RETURN TO THE FRAY."

#define C3TEXT e"YOU ARE AT THE CORRUPT HEART OF THE CITY,\n" + ;
    e"SURROUNDED BY THE CORPSES OF YOUR ENEMIES.\n" + ;
    e"YOU SEE NO WAY TO DESTROY THE CREATURES'\n" + ;
    e"ENTRYWAY ON THIS SIDE, SO YOU CLENCH YOUR\n" + ;
    e"TEETH AND PLUNGE THROUGH IT.\n" + ;
    e"\n" + ;
    e"THERE MUST BE A WAY TO CLOSE IT ON THE\n" + ;
    e"OTHER SIDE. WHAT DO YOU CARE IF YOU'VE\n" + ;
    e"GOT TO GO THROUGH HELL TO GET TO IT?"

#define C4TEXT e"THE HORRENDOUS VISAGE OF THE BIGGEST\n" + ;
    e"DEMON YOU'VE EVER SEEN CRUMBLES BEFORE\n" + ;
    e"YOU, AFTER YOU PUMP YOUR ROCKETS INTO\n" + ;
    e"HIS EXPOSED BRAIN. THE MONSTER SHRIVELS\n" + ;
    e"UP AND DIES, ITS THRASHING LIMBS\n" + ;
    e"DEVASTATING UNTOLD MILES OF HELL'S\n" + ;
    e"SURFACE.\n" + ;
    e"\n" + ;
    e"YOU'VE DONE IT. THE INVASION IS OVER.\n" + ;
    e"EARTH IS SAVED. HELL IS A WRECK. YOU\n" + ;
    e"WONDER WHERE BAD FOLKS WILL GO WHEN THEY\n" + ;
    e"DIE, NOW. WIPING THE SWEAT FROM YOUR\n" + ;
    e"FOREHEAD YOU BEGIN THE LONG TREK BACK\n" + ;
    e"HOME. REBUILDING EARTH OUGHT TO BE A\n" + ;
    e"LOT MORE FUN THAN RUINING IT WAS.\n"

#define C5TEXT e"CONGRATULATIONS, YOU'VE FOUND THE SECRET\n" + ;
    e"LEVEL! LOOKS LIKE IT'S BEEN BUILT BY\n" + ;
    e"HUMANS, RATHER THAN DEMONS. YOU WONDER\n" + ;
    e"WHO THE INMATES OF THIS CORNER OF HELL\n" + ;
    e"WILL BE."

#define C6TEXT e"CONGRATULATIONS, YOU'VE FOUND THE\n" + ;
    e"SUPER SECRET LEVEL!  YOU'D BETTER\n" + ;
    e"BLAZE THROUGH THIS ONE!\n"

#define P1TEXT e"You gloat over the steaming carcass of the\n" + ;
    e"Guardian.  With its death, you've wrested\n" + ;
    e"the Accelerator from the stinking claws\n" + ;
    e"of Hell.  You relax and glance around the\n" + ;
    e"room.  Damn!  There was supposed to be at\n" + ;
    e"least one working prototype, but you can't\n" + ;
    e"see it. The demons must have taken it.\n" + ;
    e"\n" + ;
    e"You must find the prototype, or all your\n" + ;
    e"struggles will have been wasted. Keep\n" + ;
    e"moving, keep fighting, keep killing.\n" + ;
    e"Oh yes, keep living, too."

#define P2TEXT e"Even the deadly Arch-Vile labyrinth could\n" + ;
    e"not stop you, and you've gotten to the\n" + ;
    e"prototype Accelerator which is soon\n" + ;
    e"efficiently and permanently deactivated.\n" + ;
    e"\n" + ;
    e"You're good at that kind of thing."

#define P3TEXT e"You've bashed and battered your way into\n" + ;
    e"the heart of the devil-hive.  Time for a\n" + ;
    e"Search-and-Destroy mission, aimed at the\n" + ;
    e"Gatekeeper, whose foul offspring is\n" + ;
    e"cascading to Earth.  Yeah, he's bad. But\n" + ;
    e"you know who's worse!\n" + ;
    e"\n" + ;
    e"Grinning evilly, you check your gear, and\n" + ;
    e"get ready to give the bastard a little Hell\n" + ;
    e"of your own making!"

#define P4TEXT e"The Gatekeeper's evil face is splattered\n" + ;
    e"all over the place.  As its tattered corpse\n" + ;
    e"collapses, an inverted Gate forms and\n" + ;
    e"sucks down the shards of the last\n" + ;
    e"prototype Accelerator, not to mention the\n" + ;
    e"few remaining demons.  You're done. Hell\n" + ;
    e"has gone back to pounding bad dead folks \n" + ;
    e"instead of good live ones.  Remember to\n" + ;
    e"tell your grandkids to put a rocket\n" + ;
    e"launcher in your coffin. If you go to Hell\n" + ;
    e"when you die, you'll need it for some\n" + ;
    e"final cleaning-up ..."

#define P5TEXT e"You've found the second-hardest level we\n" + ;
    e"got. Hope you have a saved game a level or\n" + ;
    e"two previous.  If not, be prepared to die\n" + ;
    e"aplenty. For master marines only."

#define P6TEXT e"Betcha wondered just what WAS the hardest\n" + ;
    e"level we had ready for ya?  Now you know.\n" + ;
    e"No one gets out alive."

#define T1TEXT e"You've fought your way out of the infested\n" + ;
    e"experimental labs.   It seems that UAC has\n" + ;
    e"once again gulped it down.  With their\n" + ;
    e"high turnover, it must be hard for poor\n" + ;
    e"old UAC to buy corporate health insurance\n" + ;
    e"nowadays..\n" + ;
    e"\n" + ;
    e"Ahead lies the military complex, now\n" + ;
    e"swarming with diseased horrors hot to get\n" + ;
    e"their teeth into you. With luck, the\n" + ;
    e"complex still has some warlike ordnance\n" + ;
    e"laying around."

#define T2TEXT e"You hear the grinding of heavy machinery\n" + ;
    e"ahead.  You sure hope they're not stamping\n" + ;
    e"out new hellspawn, but you're ready to\n" + ;
    e"ream out a whole herd if you have to.\n" + ;
    e"They might be planning a blood feast, but\n" + ;
    e"you feel about as mean as two thousand\n" + ;
    e"maniacs packed into one mad killer.\n" + ;
    e"\n" + ;
    e"You don't plan to go down easy."

#define T3TEXT e"The vista opening ahead looks real damn\n" + ;
    e"familiar. Smells familiar, too -- like\n" + ;
    e"fried excrement. You didn't like this\n" + ;
    e"place before, and you sure as hell ain't\n" + ;
    e"planning to like it now. The more you\n" + ;
    e"brood on it, the madder you get.\n" + ;
    e"Hefting your gun, an evil grin trickles\n" + ;
    e"onto your face. Time to take some names."

#define T4TEXT e"Suddenly, all is silent, from one horizon\n" + ;
    e"to the other. The agonizing echo of Hell\n" + ;
    e"fades away, the nightmare sky turns to\n" + ;
    e"blue, the heaps of monster corpses start \n" + ;
    e"to evaporate along with the evil stench \n" + ;
    e"that filled the air. Jeeze, maybe you've\n" + ;
    e"done it. Have you really won?\n" + ;
    e"\n" + ;
    e"Something rumbles in the distance.\n" + ;
    e"A blue light begins to glow inside the\n" + ;
    e"ruined skull of the demon-spitter."

#define T5TEXT e"What now? Looks totally different. Kind\n" + ;
    e"of like King Tut's condo. Well,\n" + ;
    e"whatever's here can't be any worse\n" + ;
    e"than usual. Can it?  Or maybe it's best\n" + ;
    e"to let sleeping gods lie.."

#define T6TEXT e"Time for a vacation. You've burst the\n" + ;
    e"bowels of hell and by golly you're ready\n" + ;
    e"for a break. You mutter to yourself,\n" + ;
    e"Maybe someone else can kick Hell's ass\n" + ;
    e"next time around. Ahead lies a quiet town,\n" + ;
    e"with peaceful flowing water, quaint\n" + ;
    e"buildings, and presumably no Hellspawn.\n" + ;
    e"\n" + ;
    e"As you step off the transport, you hear\n" + ;
    e"the stomp of a cyberdemon's iron shoe."

#define CC_ZOMBIE e"ZOMBIEMAN"
#define CC_SHOTGUN e"SHOTGUN GUY"
#define CC_HEAVY e"HEAVY WEAPON DUDE"
#define CC_IMP e"IMP"
#define CC_DEMON e"DEMON"
#define CC_LOST e"LOST SOUL"
#define CC_CACO e"CACODEMON"
#define CC_HELL e"HELL KNIGHT"
#define CC_BARON e"BARON OF HELL"
#define CC_ARACH e"ARACHNOTRON"
#define CC_PAIN e"PAIN ELEMENTAL"
#define CC_REVEN e"REVENANT"
#define CC_MANCU e"MANCUBUS"
#define CC_ARCH e"ARCH-VILE"
#define CC_SPIDER e"THE SPIDER MASTERMIND"
#define CC_CYBER e"THE CYBERDEMON"
#define CC_HERO e"OUR HERO"

#endif

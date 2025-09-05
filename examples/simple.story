(* Simple Story Mode example *)
@scene

## tavern_entrance

The tavern is dimly lit and filled with the smell of ale.

Bartender: "Welcome, stranger! What brings you here?"

* [Order a drink] -> order_drink
* [Ask about rumors] -> ask_rumors
* [Leave quietly] -> exit

## order_drink

You order a mug of ale.

[if gold >= 5]
  Bartender: "That'll be 5 gold pieces."
  $gold = gold - 5
  +ale
  -> drink_received

[if gold < 5]
  Bartender: "You don't have enough gold!"
  -> tavern_entrance

## drink_received

You receive your drink and take a sip.

@play_sound gulp.mp3

The ale is surprisingly good!

-> tavern_entrance

## ask_rumors

Bartender: "Well, there's talk of a dragon in the mountains..."

? perception check DC 15
  => You notice the bartender seems nervous when mentioning the dragon.
  =| The bartender continues polishing glasses.

-> tavern_entrance

## exit

You leave the tavern.

@scene_end
@scene

## dungeon_entrance
You stand before a dark dungeon entrance. The air is thick with danger.

? perception DC 12
  => You notice fresh footprints leading inside. Someone was here recently.
  =| The entrance looks undisturbed.

The heavy iron door stands before you.

? strength DC 15
  => You force the door open with a mighty push!
     -> dungeon_interior
  =| The door won't budge. You'll need to find another way.
     -> find_alternative

* [Search for a key] -> search_key
* [Try to pick the lock] -> pick_lock
* [Give up] -> leave

## dungeon_interior
You've entered the dungeon! Torches flicker on the walls.

? stealth DC 14
  => You move silently through the shadows.
  =| Your footsteps echo loudly! The guards are alerted!
     -> combat

The passage continues deeper underground.
-> treasure_room

## find_alternative
You search around the entrance for another way in.

? investigation DC 10
  => You find a hidden lever behind some vines!
     -> secret_entrance
  =| Nothing useful here.

-> dungeon_entrance

## secret_entrance
A hidden door slides open, revealing a narrow passage.
-> dungeon_interior

## search_key
You search the area around the entrance.

? investigation DC 8
  => You find an old rusty key under a loose stone!
     +dungeon_key
     -> dungeon_entrance
  =| You find nothing useful.
     -> dungeon_entrance

## pick_lock
You attempt to pick the lock.

? lockpicking DC 18
  => *Click* The lock opens!
     -> dungeon_interior
  =| The lock is too complex. You break your lockpick.
     -lockpick
     -> dungeon_entrance

## combat
Three guards rush toward you!

? combat DC 16
  => You defeat the guards in a swift battle!
     +guard_armor
     +gold
     $reputation = reputation + 5
  =| You're overwhelmed and forced to retreat!
     -> dungeon_entrance

The way ahead is now clear.
-> treasure_room

## treasure_room
You've reached the treasure room!

? trap_detection DC 14
  => You spot a pressure plate trap and carefully avoid it.
  =| You trigger a trap!
     $health = health - 10

A magnificent chest sits in the center of the room.
+ancient_artifact
+gold

@show_notification %{ui.notification.quest_complete} Quest Complete: Dungeon Delver!

-> leave

## leave
You leave the dungeon, your adventure complete.
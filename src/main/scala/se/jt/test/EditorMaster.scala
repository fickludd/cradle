package se.jt.test

import se.jt.Cradle
import se.jt.CradleMaster
import se.jt.StyleLoader
import se.jt.BorderPiece
import se.jt.ButtonPiece
import se.jt.InputPiece
import se.jt.ReadPiece
import se.jt.EditorPiece
import se.jt.NorthSouthSplit
import se.jt.EastWestSplit
import se.jt.ScrollPiece

import se.jt.frame.Compass

import se.jt.event.Reactor
import se.jt.event.ReloadStyle

import akka.actor.ActorRef

class EditorMaster(styleLoader:ActorRef) extends CradleMaster with Reactor {

	val leftGuide = ReadPiece("left", """^c   copy line
^v   paste line
^w   write to file""")
	val rightGuide = ReadPiece("right", """^x   exit
^f   find
""")
	
	lazy val cradle = new Cradle(
			"an editor",
			BorderPiece("main",
				Compass.NORTH -> NorthSouthSplit("top",
					EastWestSplit("guide", 
							leftGuide, 
							rightGuide),
					InputPiece("commandline")
						),
				Compass.WEST -> ReadPiece("lineNumbers"),
				Compass.CENTER -> EditorPiece("text")
			))
	
	listenTo(cradle)
	reactions += {
		case ReloadStyle() =>
			styleLoader ! StyleLoader.ReloadStyle()
	}
	
	styleLoader ! StyleLoader.ReloadStyle()
}
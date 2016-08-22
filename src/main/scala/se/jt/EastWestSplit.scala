package se.jt

import java.awt.Graphics2D

import se.jt.frame.Piece
import se.jt.frame.PurePoserPiece
import se.jt.frame.Configurable

object EastWestSplit {
	def apply(name:String, n:Piece, s:Piece) =
		new EastWestSplit(name, n, s)
}


class EastWestSplit(
		val name:String,
		val east:Piece,
		val west:Piece
) extends PurePoserPiece with Configurable {

	import se.jt.frame.Compass._
	
	lazy val pieces = Map(east.name -> east, west.name -> west)
	
	def framedRepose(x:Int, y:Int, w:Int, h:Int):Unit = {
		val epw = east.preferredSize._1
		
		val wpw = west.preferredSize._1
		
		val ew = ((epw.toDouble / (epw+wpw)) * w).toInt
	
		east.pos = (x, y)
		east.size = (ew, h)
		
		west.pos = (x + ew, y)
		west.size = (w - ew, h)
	}
}
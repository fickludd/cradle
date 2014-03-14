package se.jt

import java.awt.Graphics2D

import se.jt.frame.Piece
import se.jt.frame.PoserPiece

object EastWestSplit {
	def apply(name:String, n:Piece, s:Piece) =
		new NorthSouthSplit(name, n, s)
}


class EastWestSplit(
		val name:String,
		val east:Piece,
		val west:Piece
) extends PoserPiece {

	import se.jt.frame.Compass._
	
	lazy val pieces = Map(EAST.name -> east, WEST.name -> west)
	
	def repose():Unit = {
		val epw = east.preferredSize._1
		
		val wpw = west.preferredSize._1
		
		val ew = ((epw.toDouble / (epw+wpw)) * w).toInt
	
		east.pos = (x, y)
		east.size = (ew, h)
		
		west.pos = (x + ew, y)
		west.size = (w - ew, h)
	}
	
	def rerender(g:Graphics2D):Unit = {}
}
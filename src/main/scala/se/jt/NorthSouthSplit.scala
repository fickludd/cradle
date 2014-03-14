package se.jt

import java.awt.Graphics2D

import se.jt.frame.Piece
import se.jt.frame.PoserPiece

object NorthSouthSplit {
	def apply(name:String, n:Piece, s:Piece) =
		new NorthSouthSplit(name, n, s)
}


class NorthSouthSplit(
		val name:String,
		val north:Piece,
		val south:Piece
) extends PoserPiece {

	import se.jt.frame.Compass._
	
	lazy val pieces = Map(NORTH.name -> north, SOUTH.name -> south)
	
	def repose():Unit = {
		val nph = north.preferredSize._2
		val sph = south.preferredSize._2
		
		val nh = ((nph.toDouble / (nph+sph)) * h).toInt
	
		north.pos = (x, y)
		north.size = (w, nh)
		
		south.pos = (x, y + nh)
		south.size = (w, h - nh)
	}
	
	def rerender(g:Graphics2D):Unit = {}
}
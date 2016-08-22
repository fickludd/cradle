package se.jt

import java.awt.Graphics2D

import se.jt.frame.Piece
import se.jt.frame.PoserPiece
import se.jt.frame.Configurable

object NorthSouthSplit {
	def apply(name:String, n:Piece, s:Piece) =
		new NorthSouthSplit(name, n, s)
}


class NorthSouthSplit(
		val name:String,
		val north:Piece,
		val south:Piece
) extends PoserPiece with Configurable {

	import se.jt.frame.Compass._
	
	lazy val pieces = Map(north.name -> north, south.name -> south)
	
	def framedRepose(x:Int, y:Int, w:Int, h:Int):Unit = {
		val nph = north.preferredSize._2
		val sph = south.preferredSize._2
		
		val nh = ((nph.toDouble / (nph+sph)) * h).toInt
	
		north.pos = (x, y)
		north.size = (w, nh)
		
		south.pos = (x, y + nh)
		south.size = (w, h - nh)
	}
	
	def rerender(g:Graphics2D, x:Int, y:Int, w:Int, h:Int):Unit = {}
}
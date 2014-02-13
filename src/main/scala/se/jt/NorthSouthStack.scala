package se.jt

import java.awt.Color
import java.awt.Graphics2D
import java.awt.Adjustable._

import scala.util.Random
import se.jt.frame.PoserPiece
import se.jt.frame.Piece

object NorthSouthStack {
	def apply(name:String, children:Piece*) =
		new NorthSouthStack(name, children)
}

class NorthSouthStack(
		val name:String, 
		val children:Seq[Piece]
) extends PoserPiece {

	lazy val pieces = children.map(t => (t.name, t)).toMap
	
	def repose():Unit = {
		val dh = h / pieces.size
		var i = 0
		for (p <- pieces.values) {
			p.pos = (x, y + dh*i)
			p.size = (w, dh)
			i+=1
		}
	}
	
	
	def rerender(g:Graphics2D):Unit = {}
}
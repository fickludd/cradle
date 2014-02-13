package se.jt

import java.awt.Color
import java.awt.Graphics2D
import java.awt.Adjustable._

import scala.util.Random
import se.jt.frame.PoserPiece
import se.jt.frame.Piece

object EastWestStack {
	def apply(name:String, children:Piece*) =
		new EastWestStack(name, children)
}

class EastWestStack(
		val name:String, 
		val children:Seq[Piece]
) extends PoserPiece {

	lazy val pieces = children.map(t => (t.name, t)).toMap
	
	def repose():Unit = {
		val dw = w / pieces.size
		var i = 0
		for (p <- pieces.values) {
			p.pos = (x + dw*i, y)
			p.size = (dw, h)
			i+=1
		}
	}
	
	
	def rerender(g:Graphics2D):Unit = {}
}
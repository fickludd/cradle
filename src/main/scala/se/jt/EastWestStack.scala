package se.jt

import java.awt.Color
import java.awt.Graphics2D
import java.awt.Adjustable._

import scala.util.Random
import se.jt.frame.PurePoserPiece
import se.jt.frame.Piece
import se.jt.frame.Configurable

object EastWestStack {
	def apply(name:String, children:Piece*) =
		new EastWestStack(name, children)
}

class EastWestStack(
		val name:String, 
		val children:Seq[Piece]
) extends PurePoserPiece with Configurable {

	lazy val pieces = children.map(t => (t.name, t)).toMap
	
	def framedRepose(x:Int, y:Int, w:Int, h:Int):Unit = {
		val dw = w / pieces.size
		var i = 0
		for (p <- pieces.values) {
			p.pos = (x + dw*i, y)
			p.size = (dw, h)
			i+=1
		}
	}
}
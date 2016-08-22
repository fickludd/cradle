package se.jt

import java.awt.Color
import java.awt.Graphics2D
import se.jt.frame.Piece
import se.jt.frame.Colors
import se.jt.frame.Geom.Frame

import scala.util.Random

object Filler {
	def apply(pw:Int, ph:Int) = new Filler(pw, ph)
}

class Filler(
		prefWidth:Int,
		prefHeight:Int
) extends Piece {
	
	override def defaultPW = prefWidth
	override def defaultPH = prefHeight
	
	val bgColor = Some(Colors.random)
	val borderColor = Some(Color.WHITE)
	_border = Frame(1)
	
	def randChars(n:Int) =
		((0 until n) map (x => Random.nextPrintableChar)).mkString("")
	
	val name = "f"+pw+"x"+ph+" "+randChars(6)
	
	def rerender(g:Graphics2D, x:Int, y:Int, w:Int, h:Int):Unit = {
		g.setColor(Color.WHITE)
		g.drawLine(x+1, y+1, x+w-2, y+h-2)
		g.drawLine(x+w-2, y+1, x+1, y+h-2)
	}
}
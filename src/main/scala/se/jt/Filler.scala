package se.jt

import java.awt.Color
import java.awt.Graphics2D
import se.jt.frame.Piece
import se.jt.frame.Colors

import scala.util.Random

object Filler {
	def apply(pw:Int, ph:Int) = new Filler(pw, ph)
}

class Filler(
		prefWidth:Int,
		prefHeight:Int
) extends Piece {
	
	pw = prefWidth
	ph = prefHeight
	var bgColor = Colors.random
	
	def randChars(n:Int) =
		((0 until n) map (x => Random.nextPrintableChar)).mkString("")
	
	val name = "f"+pw+"x"+ph+" "+randChars(6)
	
	def rerender(g:Graphics2D):Unit = {
		val (w, h) = size
		
		g.setColor(bgColor)
		g.fillRect(x, y, w, h)
		
		g.setColor(Color.WHITE)
		g.drawRect(x+1, y+1, w-3, h-3)
		g.drawLine(x+1, y+1, x+w-2, y+h-2)
		g.drawLine(x+w-2, y+1, x+1, y+h-2)
	}
}
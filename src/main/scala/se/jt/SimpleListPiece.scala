package se.jt

import java.awt.Graphics2D
import java.awt.Color

import se.jt.frame.ListPiece
import se.jt.frame.Configurable

object SimpleListPiece {
	def apply[T](name:String, f:T => String) = new SimpleListPiece(name, f)
}

class SimpleListPiece[T](
		val name:String,
		val f:T => String
) extends ListPiece[T] with Configurable.Text {
	def itemHeight(t:T, g:Graphics2D):Int = {
		g.setFont(font)
		g.getFontMetrics().getHeight()
	}
	def renderItem(
			t:T, 
			g:Graphics2D, 
			x:Int, 
			y:Int, 
			selected:Boolean,
			odd:Boolean
	):Unit = {
		g.setFont(font)
		val fm = g.getFontMetrics()
		val th = fm.getHeight()
		
		if (selected) {
			g.setPaint(Color.RED)
			g.fillRect(x, y, w, th)
		}
		g.setPaint(Color.WHITE)
		g.drawString(f(t), x + 5, y + fm.getAscent + (th-th) / 2)
	}
}
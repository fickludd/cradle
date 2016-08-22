package se.jt

import java.awt.Graphics2D
import java.awt.Color

import se.jt.frame.Colors
import se.jt.frame.Piece
import se.jt.frame.Geom
import se.jt.frame.Configurable

object ProgressBarPiece {
	def apply(name:String, p:Double = 0.0) = new ProgressBarPiece(name, p)
}

class ProgressBarPiece(
		val name:String, 
		var _p:Double = 0.0
) extends Piece with Configurable.Text {
	
	var doneColor = Colors.lighter(Color.GREEN)
	var leftColor = Colors.darker(Color.GREEN)
	
	_margin = new Geom.Frame(3,3,3,3)
	
	def setProgress(p:Double) = {
		_p = p
		makeDirty
	}
	
	def rerender(g:Graphics2D, x:Int, y:Int, w:Int, h:Int) = {
		val pw = (w * _p).toInt
		g.setPaint(doneColor)
		g.fillRect(x, y, pw, h)
		g.setPaint(leftColor)
		g.fillRect(x+pw, y, w-pw, h)
		
		g.setPaint(textColor)
		val str = "%.1f%%".format(_p*100)
		val fm = g.getFontMetrics()
		val tw = fm.charsWidth(str.toCharArray, 0, str.length)
		val ty = y + h/2 + fm.getAscent - fm.getHeight/2
		g.drawString(str, x + w/2 - tw/2, ty)
	}
}
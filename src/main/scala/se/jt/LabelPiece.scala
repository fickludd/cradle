package se.jt

import scala.util.Random
import java.awt.Color
import java.awt.Graphics2D
import java.awt.BasicStroke

import se.jt.frame.Piece
import se.jt.frame.Configurable
import se.jt.frame.Colors

object LabelPiece {
	val HIGHLIGHT_STROKE = new BasicStroke(2.0f)
	val NORMAL_STROKE = new BasicStroke(1.0f)
	
	def apply(name:String) = new LabelPiece(name)
}

class LabelPiece(val name:String) extends Piece with Configurable.Text {
	
	import LabelPiece._
	import se.jt.frame.Compass._
	
	_bgColor = Colors.random
	
	def rerender(g:Graphics2D, x:Int, y:Int, w:Int, h:Int):Unit = {
		val fm = g.getFontMetrics()
		val tw = fm.charsWidth(name.toCharArray, 0, name.length)
		val th = fm.getHeight()
		
		val cx = eastWestAlign match {
			case WEST => x + kerning
			case CENTER => x + w/2 - tw/2
			case EAST => x + w - kerning - tw
		}
		val cy = northSouthAlign match {
			case NORTH => y + kerning
			case CENTER => y + h/2 + fm.getAscent - th / 2
			case SOUTH => y + h - kerning - th
		}
		
		g.setPaint(textColor)
		g.drawString(name, cx, cy)
	}
}
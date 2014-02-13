package se.jt

import scala.util.Random
import java.awt.Color
import java.awt.Graphics2D
import java.awt.BasicStroke

import se.jt.frame.Piece
import se.jt.frame.Configurable

object LabelPiece {
	val HIGHLIGHT_STROKE = new BasicStroke(2.0f)
	val NORMAL_STROKE = new BasicStroke(1.0f)
	
	def apply(name:String) = new LabelPiece(name)
}

class LabelPiece(val name:String) extends Piece with Configurable {
	
	import LabelPiece._

	configs += (
		("bgColor", x => bgColor = new Color(x.toInt)),
		("textColor", x => textColor = new Color(x.toInt))
	)
	
	def rFloat = Random.nextFloat / 2 + 0.25f
	var textColor = Color.BLACK
	var bgColor = {
		val r = Random.nextFloat
		val g = Random.nextFloat
		new Color(r, g, 1.0f - (r+g)/2)
	}
	
	def rerender(g:Graphics2D):Unit = {
		g.setPaint(bgColor)
		g.fillRect(x, y, w, h)
		
		/*
		if (selected) {
			g.setPaint(Color.WHITE)
			g.drawRect(x,y,w-1,h-1)
		} else if (highlighted) {
			g.setPaint(Color.RED)
			g.setStroke(HIGHLIGHT_STROKE)
			g.drawRect(x,y,w-1,h-1)
			g.setStroke(NORMAL_STROKE)
		}
		*/
		g.setPaint(Color.BLACK)
		g.drawString(name, x + 5, y + 10 + h/2)
	}
}
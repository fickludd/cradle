package se.jt

import se.jt.frame.Scrollable
import se.jt.text.Typeset
import se.jt.event.MouseResponsive

import scala.util.Random
import java.awt.Color
import java.awt.Graphics2D
import java.awt.BasicStroke

object TextPiece {
	def apply(name:String) = new TextPiece(name)
	def apply(name:String, text:String) = {
		val tp = new TextPiece(name)
		tp.text(text)
		tp
	}
}

class TextPiece(val name:String) extends Scrollable with MouseResponsive {

	var _text = ""
	var _typesetted = false
	var _lines:Seq[String] = Nil
	var _preferredHeight = -1
	
	def text(str:String) = {
		_text = str
		_typesetted = false
		makeDirty
	}
	
	def rFloat = Random.nextFloat / 2 + 0.25f
	val bg = {
		val r = Random.nextFloat
		val g = Random.nextFloat
		new Color(r, g, 1.0f - (r+g)/2)
	}
	
	override def preferredSize = 
		if (_preferredHeight > 0) Some(w, _preferredHeight)
		else None
	
	def rerender(g:Graphics2D):Unit = {
		val fm = g.getFontMetrics()
		val fontHeight = fm.getHeight()
		g.setPaint(bg)
		g.fillRect(x, y, vw, vh)
		
		if (!_typesetted && _text != "") {
			val L = new Typeset.NodeList
			L.addParagraph(_text, word => fm.charsWidth(word.toArray, 0, word.length))
			val breaks = L.computeBreakpoints( List[Int]().padTo(200, w),5, 20)
			_lines = breaks.zip(breaks.tail).map(t => {
	        	(L.slice(t._1, t._2).collect { case Typeset.Box(wi, text) => text }).mkString(" ")
	        })
	        _preferredHeight = _lines.length * fontHeight
	        _typesetted = true
		}
		
		if (selected) {
			g.setPaint(Color.WHITE)
			g.drawRect(x,y,w-1,h-1)
		} else if (active) {
			g.setPaint(Color.RED)
			g.drawRect(x,y,w-1,h-1)
		}
		
		g.setPaint(Color.BLACK)
		
		var ty = 0
		for (i <- 0 until _lines.length) {
			if (ty < vy + vh && ty + fontHeight > vy)
				g.drawString(_lines(i), x + fm.getLeading - vx, 
							y + ty + fm.getAscent - vy)
			ty += fontHeight
		}
	}
}
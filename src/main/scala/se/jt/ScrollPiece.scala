package se.jt

import java.awt.Color
import java.awt.Graphics2D

import se.jt.frame.Scrollable
import se.jt.frame.PoserPiece
import se.jt.frame.Poser
import se.jt.frame.UserInput


class ScrollPiece(
		val name:String, 
		val piece:Scrollable
) extends PoserPiece {
	
	val nsScrollBar = new NorthSouthScrollBar(piece)
	var nsScrollBarWidth = 8
	
	lazy val pieces = Map(piece.name -> piece, nsScrollBar.name -> nsScrollBar) 
	
	def repose():Unit = {
		piece.pos = (x, y)
		val (piw, pih) = piece.size
		piece.size = (
				w-nsScrollBarWidth, 
				pih)
		piece.setViewPortSize(w - nsScrollBarWidth, h)
		nsScrollBar.pos = (x+w-nsScrollBarWidth, y)
		nsScrollBar.size = (nsScrollBarWidth, h)
	}
	
	def rerender(g:Graphics2D) = {}
	
	
	override def renderTree(g:Graphics2D, t:Long) = {
		val vp = piece.viewPort
		g.clipRect(x, y, w, h)
		piece match {
			case pos:Poser => pos.renderTree(g, t)
			case _ => piece.render(g, t)
		}
		g.setClip(null)
		nsScrollBar.render(g, t)
	}
}
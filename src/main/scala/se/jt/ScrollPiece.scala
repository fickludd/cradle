package se.jt

import java.awt.Color
import java.awt.Graphics2D

import se.jt.frame.Scrollable
import se.jt.frame.PurePoserPiece
import se.jt.frame.Poser
import se.jt.input.UserInput
import se.jt.frame.Configurable
import se.jt.frame.Geom.Frame
import se.jt.event.Reactor
import se.jt.event.PreferredSizeChanged


class ScrollPiece[T <: Scrollable](
		val name:String, 
		val piece:T
) extends PurePoserPiece with Configurable with Reactor {
	
	val nsScrollBar = new NorthSouthScrollBar(piece)
	var nsScrollBarWidth = 8
	val scrollBorder = Frame(1)
	
	lazy val pieces = Map(piece.name -> piece, nsScrollBar.name -> nsScrollBar) 
	
	listenTo(pieces.values.toSeq:_*)
	reactions += {
		case PreferredSizeChanged(p) => makeDirty
	}
	
	def framedRepose(x:Int, y:Int, w:Int, h:Int):Unit = {
		piece.pos = (x, y)
		piece.setViewPortSize(w, h)
		val (piw, pih) = piece.preferredSize
		if (pih > h) {
			piece.size = (
					w-nsScrollBarWidth, 
					pih)
			piece.setViewPortSize(w - nsScrollBarWidth, h)
			nsScrollBar.pos = (x+w-nsScrollBarWidth, y)
			nsScrollBar.size = (nsScrollBarWidth, h)
			_border = scrollBorder
		} else {
			piece.size = (w, h)
			nsScrollBar.size = (0,0)
			_border = Frame.NO
		}
	}
	
	
	override def renderTree(g:Graphics2D, underlyingColor:Color, t:Long) = {
		if (renderBefore) render(g, bgColor.getOrElse(underlyingColor), t)
		val contRect = contentRect
		g.clipRect(contRect.x, contRect.y, contRect.w - nsScrollBar.size._1, contRect.h)
		piece match {
			case pos:Poser => pos.renderTree(g, bgColor.getOrElse(underlyingColor), t)
			case _ => piece.render(g, bgColor.getOrElse(underlyingColor), t)
		}
		g.setClip(null)
		nsScrollBar.render(g, bgColor.getOrElse(underlyingColor), t)
		if (!renderBefore) render(g, bgColor.getOrElse(underlyingColor), t)
	}
}
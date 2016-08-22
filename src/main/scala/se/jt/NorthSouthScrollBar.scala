package se.jt

import java.awt.Graphics2D
import java.awt.Color

import se.jt.frame.Scrollable
import se.jt.frame.Piece
import se.jt.frame.Configurable
import se.jt.input.MouseResponsive
import se.jt.input.UserInput

import se.jt.input.MouseResponsive
import se.jt.input.MouseDraggable

class NorthSouthScrollBar(
		val piece:Scrollable
) extends Piece with MouseResponsive with MouseDraggable with Configurable {

	val name = "ns-scrollbar"
	
	_bgColor = Color.GRAY
	
	var dragStartViewPort = (0,0,0,0)
	def startDrag(mx:Int, my:Int) = {
		val (sx0, sy0, sxn, syn) = sliderRect
		my >= sy0 && my <= syn
	}
	def onDragStart = 
		dragStartViewPort = piece.viewPort
	override def onDrag(mx:Int, my:Int) = {
		val (vx, vy, vw, vh) = dragStartViewPort
		val (pw, ph) = piece.preferredSize
		if (ph > vh) {
			piece.setViewPortPos(vx, vy + ((my - dragStartPos._2)*ph)/h)
			makeDirty
		}
	}
	
	def sliderRect = {
		val (vx, vy, vw, vh) 	= piece.viewPort
		val (pw, ph) 			= piece.preferredSize
		val bar_start 	= (vy.toDouble / ph * h + y).toInt 
		val bar_end 	= ((vy.toDouble + vh) / ph * h + y).toInt
		
		(x+1, math.max(y+1, bar_start), x+w-1, math.min(y+h-1, bar_end))
	}
	
	def rerender(g:Graphics2D, x:Int, y:Int, w:Int, h:Int):Unit = {
		val (sx0, sy0, sxn, syn) = sliderRect
		g.setPaint(if (active) Color.WHITE else Color.LIGHT_GRAY)
		g.fillRect(sx0, sy0, w-2, syn - sy0)
	}
}
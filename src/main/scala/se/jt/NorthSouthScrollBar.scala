package se.jt

import java.awt.Graphics2D
import java.awt.Color

import se.jt.frame.Scrollable
import se.jt.frame.Piece
import se.jt.event.MouseResponsive
import se.jt.frame.UserInput

class NorthSouthScrollBar(
		val piece:Scrollable
) extends Piece with MouseResponsive {

	val name = "ns-scrollbar"
	
	var isDragged = false
	var dragStartMouseY = 0
	var dragStartViewPort = (0,0,0,0)
	
	import UserInput._
	captures += {
		case MousePress(mx, my) => 
			val (sx0, sy0, sxn, syn) = sliderRect
			if (my >= sy0 && my <= syn) {
				isDragged = true
				dragStartMouseY = my
				dragStartViewPort = piece.viewPort
				Lock
			} else Ignore
		case MouseRelease(mx, my, obj) =>
			isDragged = false
			UnLock
		case MouseExitCradle(mx, my) =>
			isDragged = false
			UnLock
		case MouseMove(mx, my, pressed) =>
			if (isDragged) { 
				val (vx, vy, vw, vh) = dragStartViewPort
				val (pw, ph) = piece.preferredSize
				if (ph > vh) {
					piece.setViewPortPos(vx, vy + ((my-dragStartMouseY)*ph)/h)
					makeDirty
				}
				Eat
			} else Ignore
	}
	
	def sliderRect = {
		val (vx, vy, vw, vh) 	= piece.viewPort
		val (pw, ph) 			= 
			piece.preferredSize
		val bar_start 	= (vy.toDouble / ph * h + y).toInt 
		val bar_end 	= ((vy.toDouble + vh) / ph * h + y).toInt
		
		(x+1, math.max(y+1, bar_start), x+w-1, math.min(y+h-1, bar_end))
	}
	
	def rerender(g:Graphics2D):Unit = {
		g.setPaint(Color.BLACK)
		g.fillRect(x, y, w, h)
		
		val (sx0, sy0, sxn, syn) = sliderRect
		g.setPaint(if (active) Color.WHITE else Color.LIGHT_GRAY)
		g.fillRect(sx0, sy0, w-2, syn - sy0)
	}
}
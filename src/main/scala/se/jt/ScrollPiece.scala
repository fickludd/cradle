package se.jt

import java.awt.Color
import java.awt.Graphics2D

import se.jt.frame.Scrollable
import se.jt.frame.PoserPiece
import se.jt.frame.Poser
import se.jt.frame.UserInput

import se.jt.event.MouseResponsive


class ScrollPiece(
		val name:String, 
		val piece:Scrollable
) extends PoserPiece with MouseResponsive {
	
	lazy val pieces = Map(piece.name -> piece) 
	
	var isDragged = false
	var dragStartMouseY = 0
	var dragStartViewPort = (0,0,0,0)
	
	import UserInput._
	captures += {
		case MousePress(mx, my) => 
			val (sx0, sy0, sxn, syn) = sliderRect
			if (mx >= sx0 && mx <= sxn && my >= sy0 && my <= syn) {
				isDragged = true
				dragStartMouseY = my
				dragStartViewPort = piece.viewPort
				true
			} else false
		case MouseRelease(mx, my, obj) =>
			isDragged = false
			true
		case MouseExitCradle(mx, my) =>
			isDragged = false
			false
		case MouseMove(mx, my, pressed) =>
			val (sx0, sy0, sxn, syn) = sliderRect
			if (isDragged) { 
				val (vx, vy, vw, vh) = dragStartViewPort
				val (pw, ph) = piece.size
				piece.setViewPortPos(vx, vy + ((my-dragStartMouseY)*ph)/vh)
			}
			true
	}
	
	def scroll(dx:Int, dy:Int) = {
		val vp = piece.viewPort
        piece.setViewPortPos(vp._1 + dx, vp._2 + dy)
        makeDirty
	}
	
	def repose():Unit = {
		piece.pos = (x, y)
		val ps = piece.preferredSize
		piece.size = (w-5, if (ps.isDefined) ps.get._2 else h-5)
		piece match {
			case p:Poser => p.posePieces
			case _ => {}
		}
		piece.setViewPortSize(w - 5, h - 5)
	}
	
	override def posePieces():Unit = {
		if (needRepose) {
			repose()
			needRepose = false
		}
	}
	
	def sliderRect = {
		val (vx, vy, vw, vh) 	= piece.viewPort
		val (pw, ph) 			= piece.size
		val bar_start 	= (vy.toDouble / ph * vh + y).toInt 
		val bar_end 	= ((vy.toDouble + vh) / ph * vh + y).toInt
		
		(x+w-4, math.max(y+1, bar_start), x+w-1, math.min(y+h-6, bar_end))
	}
	
	def rerender(g:Graphics2D):Unit = {
		g.setPaint(Color.BLACK)
		g.fillRect(x+w-5, y, 5, h)
		g.fillRect(x, y+h-5, w, 5)
		
		val (sx0, sy0, sxn, syn) = sliderRect
		g.setPaint(if (active) Color.WHITE else Color.LIGHT_GRAY)
		g.fillRect(sx0, sy0, 3, syn - sy0)
	}
	
	override def renderTree(g:Graphics2D, t:Long) = {
		val vp = piece.viewPort
		g.clipRect(x, y, w, h)
		piece match {
			case pos:Poser => pos.renderTree(g, t)
			case _ => piece.render(g, t)
		}
		g.setClip(null)
		if (isDirty) {
			rerender(g)
			isDirty = false
		}
	}
}
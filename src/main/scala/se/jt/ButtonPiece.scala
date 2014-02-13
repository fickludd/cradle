package se.jt

import java.awt.Color
import java.awt.Graphics2D

import se.jt.frame.Piece
import se.jt.frame.UserInput
import se.jt.frame.Colors
import se.jt.frame.Configurable
import se.jt.event.MouseResponsive
import se.jt.event.Publisher
import se.jt.event.InputDone

import se.jt.frame.Animation

object ButtonPiece {
	
	def apply(name:String) = new ButtonPiece(name)
}



class ButtonPiece(val name:String) extends Piece with Publisher with MouseResponsive with Configurable {
	
	import LabelPiece._
	
	import UserInput._

	configs += (
		("bgColor", x => bgColor = new Color(x.toInt)),
		("textColor", x => textColor = new Color(x.toInt)),
		("pressedColor", x => pressedColor = new Color(x.toInt)),
		("clickColor", x => clickColor = new Color(x.toInt))
	)
	
	var bgColor = Color.GRAY
	var textColor = Color.BLACK
	var pressedColor = Color.DARK_GRAY
	var clickColor = Color.BLUE
	
	protected var pressed = false
	
	class ClickAnimation extends Animation {
		val startTime = System.currentTimeMillis
		val duration = 300L
		
		def animate(p:Piece, g:Graphics2D, t:Long) = {
			val k = math.min(t.toDouble, duration) / duration
			g.setPaint(Colors.interpolate(clickColor, bgColor, k.toFloat))
			g.fillRect(x, y, w, h)
			
			g.setPaint(textColor)
			g.drawString(name, x + 5, y + 10 + h/2)
		}
		
		def hasEnded(r:Long) = r > duration
	}
	
	
	captures += {
		case MouseClick(x, y) if wraps(x, y) =>
			println("clicked")
			animation = Some(new ClickAnimation)
			publish(InputDone(this))
			makeDirty
			true
		
		case MousePress(x, y) if wraps(x, y)  =>
			println("pressed")
			pressed = true
			makeDirty
			true
			
		case MouseLeave() =>
			pressed = false
			makeDirty
			true
		
		/*case MouseRelease(x, y, obj) =>
			println("released")
			state = Some(MouseRelease(x, y, obj))
			makeDirty
			true
			*/
	}
	
	def rerender(g:Graphics2D):Unit = {
		
		g.setPaint(
					if (active && pressed) 	pressedColor
					else bgColor
				)
		g.fillRect(x, y, w, h)
		
		if (active) {
			g.setPaint(Color.RED)
			g.drawRect(x, y, w-1, h-1)
		}
		
		g.setPaint(textColor)
		g.drawString(name, x + 5, y + 10 + h/2)
	}

}
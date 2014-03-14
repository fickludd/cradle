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
	def apply(name:String, onClick:() => Unit) = {
		val b = new ButtonPiece(name)
		b.reactions += {
			case InputDone(b) =>
				onClick()
		}
		b
	}
	
	trait State
	case class Up() extends State
	case class Active() extends State
	case class Pressed() extends State
	case class Clicked() extends State
}



class ButtonPiece(val name:String) extends Piece with Publisher with MouseResponsive with Configurable {
	
	import LabelPiece._
	import UserInput._
	import ButtonPiece._

	configs += (
		("bgColor", x => bgColor = new Color(x.toInt)),
		("textColor", x => textColor = new Color(x.toInt)),
		("pressedColor", x => pressedColor = new Color(x.toInt)),
		("clickColor", x => clickedColor = new Color(x.toInt))
	)
	
	var bgColor = Color.GRAY
	var activeColor:Color = Colors.lighter(bgColor)
	var textColor = Color.BLACK
	var pressedColor = Color.DARK_GRAY
	var clickedColor = Color.BLUE
	
	protected var state:State = Up()
	def resetState = 
		state match {
			case Up() => {}
			case _ =>
				makeDirty
				state = Up()
		}
	
	
	class ClickAnimation extends Animation {
		val startTime = System.currentTimeMillis
		val duration = 300L
		
		def animate(p:Piece, g:Graphics2D, t:Long) = {
			val k = math.min(t.toDouble, duration) / duration
			g.setPaint(Colors.interpolate(clickedColor, bgColor, k.toFloat))
			g.fillRect(x, y, w, h)
			
			val fm = g.getFontMetrics()
		
			g.setPaint(textColor)
			g.drawString(name, x + 5, y + fm.getAscent + (fm.getHeight - h) / 2)
		}
		
		def hasEnded(r:Long) = r > duration
	}
	
	
	captures += {
		case MouseClick(x, y) if wraps(x, y) =>
			println("clicked")
			//animation = Some(new ClickAnimation)
			state = Clicked()
			publish(InputDone(this))
			makeDirty
			Eat
		
		case MousePress(x, y) if wraps(x, y)  =>
			println("pressed")
			state = Pressed()
			makeDirty
			Eat
			
		case MouseEnter() =>
			state = Active()
			makeDirty
			Eat
			
		case MouseLeave() =>
			state = Up()
			makeDirty
			Eat
		
		/*case MouseRelease(x, y, obj) =>
			println("released")
			state = Some(MouseRelease(x, y, obj))
			makeDirty
			true
			*/
	}
	
	def rerender(g:Graphics2D):Unit = {
		
		val c = 
			state match {
				case Up() => bgColor
				case Active() => activeColor
				case Pressed() => pressedColor
				case Clicked() => 
					resetState
					clickedColor
			}
			//if (active && pressed) pressedColor
			//else if (active) activeColor
			//else bgColor
		
		val (w, h) = size
		g.setPaint(c)
		g.fillRect(x, y, w, h)
		
		val bh = 5
		val bw = 2
		val dc:Color = Colors.darker(c)
		val lc:Color = Colors.lighter(c)
		g.setPaint(dc)
		g.fillRect(x, y+h-bh, w, bh)
		g.fillRect(x+w-bw, y, bw, h)
		g.setPaint(lc)
		g.fillRect(x, y, bw, h-bh/2)
		
		/*if (active) {
			g.setPaint(Color.RED)
			g.drawRect(x, y, w-1, h-1)
		}*/
		
		val fm = g.getFontMetrics()
		
		g.setPaint(textColor)
		g.drawString(name, x + 5, y + fm.getAscent + (h-fm.getHeight-bh) / 2)
	}

}
package se.jt

import java.awt.Color
import java.awt.Graphics2D

import se.jt.frame.Piece
import se.jt.input.UserInput
import se.jt.frame.Colors
import se.jt.frame.Configurable
import se.jt.frame.Icon
import se.jt.input.MouseResponsive
import se.jt.event.Publisher
import se.jt.event.InputDone

import se.jt.frame.Animation

object ButtonPiece {
	
	def apply(name:String) = new ButtonPiece(name)
	def apply(name:String, icon:Icon) = {
		val b = new ButtonPiece(name)
		b.icon_=(icon)
		b
	}
	def apply(name:String, onClick:() => Unit) = {
		val b = new ButtonPiece(name)
		b.reactions += {
			case InputDone(b) =>
				onClick()
		}
		b
	}
	def apply(name:String, icon:Icon, onClick:() => Unit) = {
		val b = new ButtonPiece(name)
		b.icon_=(icon)
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



class ButtonPiece(val name:String) extends Piece 
	with Publisher with MouseResponsive with Configurable.Text {
	
	import LabelPiece._
	import UserInput._
	import ButtonPiece._
	import se.jt.frame.Compass._

	configs += (
		("pressedColor", x => pressedColor = Some(new Color(Configurable.parseInt(x)))),
		("clickColor", x => clickedColor = Some(new Color(Configurable.parseInt(x))))
	)
	
	var activeColor:Option[Color] = None
	var pressedColor:Option[Color] = None// = Color.DARK_GRAY
	var clickedColor:Option[Color] = None// = Color.BLUE
	
	protected var _icon:Option[Icon] = None
	def icon_=(i:Icon) = {
		_icon = Some(i)
		makeDirty
	}
	def clearIcon = {
		_icon = None
		makeDirty
	}
	
	protected var state:State = Up()
	def resetState = 
		state match {
			case Up() => {}
			case _ =>
				makeDirty
				state = Up()
		}
	
	/*
	class ClickAnimation extends Animation {
		val startTime = System.currentTimeMillis
		val duration = 300L
		
		def animate(p:Piece, g:Graphics2D, t:Long) = {
			val k = math.min(t.toDouble, duration) / duration
			g.setPaint(Colors.interpolate(clickedColor, bgColor.get, k.toFloat))
			g.fillRect(x, y, w, h)
			
			val fm = g.getFontMetrics()
		
			g.setPaint(textColor)
			g.drawString(name, x + 5, y + fm.getAscent + (fm.getHeight - h) / 2)
		}
		
		def hasEnded(r:Long) = r > duration
	}
	*/
	
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
	
	def rerender(g:Graphics2D, x:Int, y:Int, w:Int, h:Int):Unit = {
		
		val c = 
			state match {
				case Up() => bgColor.get
				case Active() => 
					activeColor.getOrElse(Colors.hsv2color(Colors.darker(bgColor.get)))
				case Pressed() => 
					pressedColor.getOrElse(Colors.hsv2color(Colors.darker(Colors.darker(bgColor.get))))
				case Clicked() => 
					resetState
					clickedColor.getOrElse(Colors.hsv2color(Colors.darker(Colors.darker(bgColor.get))))
			}
			//if (active && pressed) pressedColor
			//else if (active) activeColor
			//else bgColor
		
		//val (w, h) = size
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
		
		val (iw, ih) =
			_icon match {
				case Some(i) => (i.w, i.h)
				case None => (0, 0)
			}
		
		if (_icon.nonEmpty)
			_icon.get.render(g, x+5, y + h/2 - ih/2)
		
		g.setFont(font)
		val fm = g.getFontMetrics()
		val tw = fm.charsWidth(name.toCharArray, 0, name.length)
		val th = fm.getHeight()
		
		val cx = eastWestAlign match {
			case WEST => x + kerning + iw + kerning
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
package se.jt

import java.awt.Color
import java.awt.Graphics2D

import se.jt.frame.Colors
import se.jt.frame.Piece
import se.jt.input.UserInput
import se.jt.frame.Configurable
import se.jt.input.MouseResponsive
import se.jt.event.Publisher
import se.jt.event.InputDone


object ToggleButtonPiece {
	
	def apply(name:String) = new ToggleButtonPiece(name)
	def apply(name:String, onClick:() => Unit) = {
		val b = new ToggleButtonPiece(name)
		b.reactions += {
			case InputDone(b) =>
				onClick()
		}
		b
	}
	
	trait State
	case class UpHighlight() extends State
	case class DownHighlight() extends State
	case class Up() extends State
	case class Down() extends State
	case class Pressed() extends State
}


class ToggleButtonPiece(
		val name:String
) extends Piece with Publisher 
	with MouseResponsive with Configurable.Text {

	import ToggleButtonPiece._
	import UserInput._
	
	configs += (
		("upColor", x => upColor = new Color(x.toInt)),
		("upHighlightColor", x => upHighlightColor = new Color(x.toInt)),
		("downColor", x => downColor = new Color(x.toInt)),
		("downHighlightColor", x => downHighlightColor = new Color(x.toInt)),
		("pressedColor", x => pressedColor = new Color(x.toInt))
	)
	
	var upColor = new Color(0x88CC11)
	var upHighlightColor:Color = Colors.lighter(upColor)
	var downColor = Color.GREEN
	var downHighlightColor:Color = Colors.lighter(downColor)
	var pressedColor = Color.DARK_GRAY
	
	override def bgColor = 
		Some(state match {
				case Up() => upColor
				case UpHighlight() => upHighlightColor
				case Down() => downColor
				case DownHighlight() => downHighlightColor
				case Pressed() => pressedColor
			})
	
	var isDown = false
	protected var state:State = Up()
	def up() = 
		state match {
			case Up() => {}
			case _ =>
				isDown = false
				state = Up()
				makeDirty
		}
	def down() = 
		state match {
			case Down() => {}
			case _ =>
				isDown = true
				state = Down()
				makeDirty
		}
	
	
	captures += {
		case MouseClick(x, y) if wraps(x, y) =>
			println("clicked")
			//animation = Some(new ClickAnimation)
			state =
				 if (isDown) 	Up()
				 else			Down()
			isDown = !isDown
			publish(InputDone(this))
			makeDirty
			Eat
		
		case MousePress(x, y) if wraps(x, y)  =>
			println("pressed")
			state = Pressed()
			makeDirty
			Eat
			
		case MouseEnter() =>
			state =
				 if (isDown) 	DownHighlight()
				 else			UpHighlight()
			makeDirty
			Eat
			
		case MouseLeave() =>
			state =
				 if (isDown) 	Down()
				 else			Up()
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
		
		val bh = 5
		val bw = 2
		val c:Color = bgColor.getOrElse(Color.GRAY)
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
		
		g.setFont(font)
		val fm = g.getFontMetrics()
		
		g.setPaint(textColor)
		g.drawString(name, x + 5, y + fm.getAscent + (h-fm.getHeight-bh) / 2)
	}
}
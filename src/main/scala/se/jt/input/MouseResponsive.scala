package se.jt.input

import se.jt.frame.Piece
import java.awt.Graphics2D
import java.awt.Color

trait MouseResponsive extends Piece {
	
	import UserInput._
	protected var active = false
	
	override def framedRerender(g: Graphics2D, underlyingColor:Color) = {
		super.framedRerender(g, underlyingColor)
		
		if (active) {
			g.setPaint(Color.RED)
			val mRect = rect.removeFrame(_margin)
			g.drawRect(mRect.x, mRect.y, mRect.w-1, mRect.h-1)
		}
	}
	
	captures += {
		case MouseEnter() =>
			active = true
			makeDirty
			Eat
			
		case MouseLeave() =>
			active = false
			makeDirty
			Eat
	}
}
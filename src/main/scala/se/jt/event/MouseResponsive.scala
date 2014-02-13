package se.jt.event

import se.jt.frame.UserInput
import se.jt.frame.Piece

trait MouseResponsive extends Piece {
	
	import UserInput._
	protected var active = false
	
	captures += {
		case MouseEnter() =>
			active = true
			makeDirty
			true
			
		case MouseLeave() =>
			active = false
			makeDirty
			true
	}
}
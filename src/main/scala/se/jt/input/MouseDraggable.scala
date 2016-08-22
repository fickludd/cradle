package se.jt.input

import se.jt.frame.Piece

trait MouseDraggable extends Piece {

	import UserInput._
	
	var isDragged = false
	var dragStartPos = (0,0)
	
	captures += {
		case MousePress(mx, my) => 
			if (startDrag(mx, my)) {
				isDragged = true
				onDragStart(mx, my)
				dragStartPos = (mx,my)
				Lock
			} else Ignore
		case MouseRelease(mx, my, obj) =>
			isDragged = false
			onDragEnd(mx, my)
			UnLock
		case MouseExitCradle(mx, my) =>
			isDragged = false
			onDragEnd(mx, my)
			UnLock
		case MouseMove(mx, my, pressed) =>
			if (isDragged) { 
				onDrag(mx, my)
				Eat
			} else Ignore
	}
	
	def startDrag(mx:Int, my:Int):Boolean
	def onDragStart(mx:Int, my:Int):Unit = {}
	def onDrag(mx:Int, my:Int):Unit = {}
	def onDragEnd(mx:Int, my:Int):Unit = {}
}
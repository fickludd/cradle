package se.jt.text

import se.jt.frame.UserPermission
import se.jt.frame.UserPermission._
import se.jt.input.MouseDraggable
import se.jt.input.UserInput._
import se.jt.frame.Compass

trait SelectableText extends MouseDraggable {

	def textSelected = selPos != caret
	protected var _permission:UserPermission = Edit
	var caret:TextPos = TextPos(0,0)
	var selPos:TextPos = TextPos(0,0)
	
	
	
	def movePos(pos:TextPos, n:Int):TextPos
	def moveRow(pos:TextPos, n:Int):TextPos
	def toTextPos(x:Int, y:Int):TextPos
	
	
	def startDrag(mx:Int, my:Int):Boolean = true
	override def onDragStart(mx:Int, my:Int):Unit = 
		selPos = toTextPos(mx, my)
	override def onDrag(mx:Int, my:Int):Unit = {
		caret = toTextPos(mx, my)
		makeDirty
	}
	
	captures += {
		case ARROW(d, mods) =>
			import Compass._
			if (_permission != Look) {
				d match {
					case WEST => caret = movePos(caret, -1)
					case EAST => caret = movePos(caret, 1)
					case NORTH => caret = moveRow(caret, -1)
					case SOUTH => caret = moveRow(caret, 1)
					case _ => {}
				}
				if (!mods.shift) selPos = caret
				makeDirty
				Eat
			} else
				Ignore
	}
}
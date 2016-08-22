package se.jt.text

import se.jt.frame.Piece
import se.jt.frame.Compass
import se.jt.frame.UserPermission
import se.jt.frame.UserPermission._
import se.jt.input.UserInput
import se.jt.event.TypingDone


trait Typeable extends SelectableText {
	
	import UserInput._
	
	def charFilter:Char => Boolean
	
	def deleteSelection:Unit
	def insert(pos:TextPos, text:String):Unit
	def insert(pos:TextPos, c:Char):Unit
	def hasText:Boolean
	
	def permission_=(ue:UserPermission) = {
		if (ue == Look && _permission != Look) {
			caret = TextPos(0,0)
			selPos = caret
		}
		_permission = ue
	}
	
	captures += {
		case PRINTABLECHAR(c, mods) =>
			if (charFilter(c) && _permission == Edit) {
				if (textSelected)
					deleteSelection
	
				insert(caret, c)
				caret = movePos(caret, 1)
				selPos = caret
				makeDirty
				Eat
			} else
				Ignore

		case BACKSPACE() =>
			if (hasText && _permission == Edit) {
				if (textSelected)
					deleteSelection
				else {
					selPos = movePos(caret, -1)
					deleteSelection
				}
				makeDirty
			}
			Eat
	}
}
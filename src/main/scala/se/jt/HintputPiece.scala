package se.jt

import java.awt.Graphics2D
import java.awt.Color
import java.awt.TextArea
import java.awt.event._

import se.jt.input.UserInput
import se.jt.frame.Piece
import se.jt.frame.PoserPiece
import se.jt.frame.Compass

import scala.collection.mutable.Stack

object HintputPiece {
	
	def apply(name:String, hintGen:String => Seq[String]) =
		new HintputPiece(name, hintGen)
	
	import UserInput._
	
	trait HintMode {
		def reset:Unit
		def hintsVisible:Boolean
		def updateHints:Boolean
		def onChar(c:KeyBoardInput):Unit
	}
	case class Eager extends HintMode {
		var _uh = false
		def updateHints = _uh
		def reset = _uh = false
		def hintsVisible = true
		def onChar(c:KeyBoardInput) = c match {
			case BACKSPACE() => _uh = true
			case PRINTABLECHAR(c, mods) => _uh = true
			case _ => {}
		}
	}
	case class Tab extends HintMode {
		var _uh = false
		var _hv = false
		def updateHints = _uh
		def reset = _uh = false
		def hintsVisible = _hv
		def onChar(c:KeyBoardInput) = c match {
			case TAB() => 
				_uh = true
				_hv = true
			case BACKSPACE() => _hv = false
			case PRINTABLECHAR(c, mods) => _hv = false
			case _ => {}
				
		}
	}
}

class HintputPiece(
		name:String,
		var hintGen:String => Seq[String]
) extends InputPiece(name) with PoserPiece {

	import HintputPiece._
	import UserInput._
	import Compass._
	
	var hintMode:HintMode = Eager()
	val hintList = SimpleListPiece[String]("hints", t => t)
	
	_bgColor = Color.LIGHT_GRAY
	lazy val pieces = Map[String, Piece]()
	
	def framedRepose(x:Int, y:Int, w:Int, h:Int):Unit = {
		hintList.pos = (x, y+h)
		hintList.userPrefWidth(w)
	}
	
	override def cradle(c:se.jt.Cradle) = 
		c.registerFloat(hintList)
	
	captures += {
		case PRINTABLECHAR(c, mods) =>
			hintMode.reset
			hintMode.onChar(PRINTABLECHAR(c, mods))
			if (hintMode.updateHints) {
				hintList.items_=(
					if (sb.isEmpty || !hintMode.hintsVisible)	
						Nil
					else hintGen(sb.result)
				)
				makeDirty
			}
			Eat
		
		case BACKSPACE() => 
			hintMode.reset
			hintMode.onChar(BACKSPACE())
			if (hintMode.updateHints) {
				hintList.items_=(
					if (sb.isEmpty || !hintMode.hintsVisible)	
						Nil
					else hintGen(sb.result)
				)
				makeDirty
			}
			Eat
			
		case ARROW(d, mods) =>
			d match {
				case NORTH => hintList.selUp
				case SOUTH => hintList.selDown
				case _ => {}
			}
			Eat
		
		case ENTER() =>
			if (hintList.hasSelected) {
				sb.clear
				sb ++= hintList.getSelected
			}
			Eat
	}
}
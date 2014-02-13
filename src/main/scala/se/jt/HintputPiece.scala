package se.jt

import java.awt.Graphics2D
import java.awt.Color
import java.awt.TextArea
import java.awt.event._

import scala.collection.mutable.Stack

object HintputPiece {
/*
	trait HintMode {
		def reset:Unit
		def hintsVisible:Boolean
		def updateHints:Boolean
		def onChar(c:KeyBoardCase):Unit
	}
	case class Eager extends HintMode {
		var _uh = false
		def updateHints = _uh
		def reset = _uh = false
		def hintsVisible = true
		def onChar(c:KeyBoardCase) = c match {
			case BACKSPACE() => _uh = true
			case PRINTABLECHAR(c) => _uh = true
			case _ => {}
		}
	}
	case class Tab extends HintMode {
		var _uh = false
		var _hv = false
		def updateHints = _uh
		def reset = _uh = false
		def hintsVisible = _hv
		def onChar(c:KeyBoardCase) = c match {
			case TAB() => 
				_uh = true
				_hv = true
			case BACKSPACE() => _hv = false
			case PRINTABLECHAR(c) => _hv = false
			case _ => {}
				
		}
	}
}

class HintputPiece(
		var hintGen:String => Seq[String]
) extends PoserPiece with CharReader with Publisher {
	
	import HintputPiece._
	
	var text = ""
	var caret = 0
	var selPos = 0
	var hints:Seq[String] = Nil
	var lastHintRenderSize = 0
	var hintPos = 0
	var hintMode:HintMode = Eager()
	
	override def select() = {
		selected = true
		makeDirty
	}
	
	val hintList = new SimpleListPiece[String](t => t)
	
	def repose():Unit = {
		hintList.pos = (x+5, y+h-2)
		val (pw, ph) = hintList.preferredSize.get
		hintList.size = (w - 10, ph)
	}
	
	override def deselect() = {
		selected = false
		hintList.items_=(Nil)
		makeDirty
	}
	
	def read(chars:List[KeyBoardCase]):List[KeyBoardCase] = {
		val sb = new StringBuilder(text)
		hintMode.reset
		
		for (kbc <- chars.takeWhile(_ != ENTER())) {
			kbc match {
				case BACKSPACE() => 
					if (sb.nonEmpty) {
						if (selPos != caret) {
							sb.delete(math.min(selPos, caret), math.max(selPos, caret))
							caret = math.min(selPos, caret)
						} else {
							sb.delete(caret-1, caret)
							caret -= 1
						}
						selPos = caret
					}
				case PRINTABLECHAR(c) => 
					sb.insert(caret, c)
					caret += 1
					selPos = caret
				case ARROW(d, s, a, c) =>
					d match {
						case LEFT() => caret = math.max(0, caret-1)
						case RIGHT() => caret = math.min(sb.length, caret+1)
						case UP() => hintList.selUp
						case DOWN() => hintList.selDown
						case _ => {}
					}
					if (!s)
						selPos = caret
				case _ =>
					print(".")
			}
			hintMode.onChar(kbc)
		}
		text = sb.result
		if (hintMode.updateHints) {
			hintList.items_=(
				if (text.isEmpty || !hintMode.hintsVisible)	
					Nil
				else hintGen(text)
			)
			needRepose = true
		}
		
		makeDirty
		val ret = chars.dropWhile(_ != ENTER())
		if (ret.nonEmpty) { // ENTER WAS PRESSED
			if (hintList.hasSelected)
				text = hintList.getSelected
			
			deselect
			publish(TypingDone(this))
			ret.tail
		} else ret
	}
	
	def rerender(g:Graphics2D):Unit = {
		val fm = g.getFontMetrics()
		val th = fm.getHeight()
		
		g.setPaint(Color.LIGHT_GRAY)
		g.fillRect(x, y, w, h)
		
		if (selected) {
			g.setPaint(Color.WHITE)
			g.drawRect(x,y,w-1,h-1)
		} else if (highlighted) {
			g.setPaint(Color.RED)
			g.setStroke(LabelPiece.HIGHLIGHT_STROKE)
			g.drawRect(x,y,w-1,h-1)
			g.setStroke(LabelPiece.NORMAL_STROKE)
		}
		
		val tw = fm.charsWidth(text.toCharArray, 0, text.length)
		val tx = if (tw > w) x + w - (tw+5) else x + 5
		val ty = y + h/2 + fm.getAscent - th/2
		val cx = fm.charsWidth(text.toCharArray, 0, caret)
		
		g.clipRect(x, y, w, h)
		
		if (selPos != caret) {
			val spx = fm.charsWidth(text.toCharArray, 0, selPos)
			g.setPaint(new Color(0xDDDDDD))
			g.fillRect(tx+math.min(cx, spx), ty-th, math.abs(cx - spx), (3*th)/2)
		}
		
		g.setPaint(Color.BLACK)
		g.drawString(text, tx, ty)
		
		g.setPaint(Color.WHITE)
		g.drawLine(tx+cx, ty - th, tx+cx, ty + th/2)
		
		g.setClip(null)
	}
	* */
}
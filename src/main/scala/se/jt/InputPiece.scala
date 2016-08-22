package se.jt

import java.awt.Graphics2D
import java.awt.Color
import java.awt.TextArea
import java.awt.event._
import java.awt.FontMetrics

import se.jt.frame.Piece
import se.jt.input.UserInput
import se.jt.frame.Compass
import se.jt.frame.Configurable
import se.jt.frame.Colors
import se.jt.frame.UserPermission

import se.jt.text.Fonts
import se.jt.text.Typeable
import se.jt.text.TextPos
import se.jt.event.Publisher
import se.jt.event.TypingDone
import se.jt.input.MouseResponsive

import scala.collection.mutable.Stack

object InputPiece {

	def apply(name: String) = new InputPiece(name)
}

class InputPiece(
		val name: String
) extends Piece with Publisher 
	with MouseResponsive with Typeable
	with Configurable.Text {

	
	import se.jt.frame.Compass._
	
	var charFilter:Char => Boolean = c => true

	val sb = new StringBuilder

	
	def deleteSelection = {
		sb.delete(math.min(selPos.col, caret.col), math.max(selPos.col, caret.col))
		caret = if (selPos.col < caret.col) selPos else caret
		selPos = caret
	}
	
	
	def toTextPos(mx:Int, my:Int) = {
		val fm = Fonts.getFontMetrics(font)
		val cr = contentRect
		val tw = fm.charsWidth(text.toCharArray, 0, text.length)
		TextPos(0, Fonts.toCharDel(mx - getTX(cr.x, cr.w, tw), fm, sb.result))
	}
	
	def insert(pos:TextPos, text:String) = 
		sb.insert(caret.col, text)
	def insert(pos:TextPos, c:Char) = 
		sb.insert(caret.col, c)
	def moveRow(pos:TextPos, n:Int) = pos
	def movePos(pos:TextPos, n:Int) = 
		if (n >= 0) TextPos(0, math.min(sb.length, pos.col + 1))
		else		TextPos(0, math.max(0, pos.col - 1))
	def hasText = sb.nonEmpty


	def text() = sb.result
	
	def text_=(str: String) = {
		sb.clear
		sb ++= str
		caret = TextPos(0, str.length)
		selPos = caret
		makeDirty
		publish(TypingDone(this))
	}
	
	def getTX(x:Int, w:Int, tw:Int) =
		if (tw > w) x + w - tw
		else 
		 eastWestAlign match {
			case WEST => x
			case CENTER => x + w/2 - tw/2
			case EAST => x + w - tw
		}
	def getTY(y:Int, h:Int, fm:FontMetrics) =
		northSouthAlign match {
			case NORTH => y
			case CENTER => y + h/2 + fm.getAscent - fm.getHeight / 2
			case SOUTH => y + h - fm.getHeight
		}

	import UserInput._
	import UserPermission._
	captures += {
		
		case ENTER() =>
			/// will focus move upon typing complete?
			if (_permission == Edit) {
				publish(TypingDone(this))
				println("TYping done!")
				Eat
			} else Ignore
	}
	
	def rerender(g: Graphics2D, x:Int, y:Int, w:Int, h:Int): Unit = {
		g.clipRect(x, y, w, h)

		val text = sb.result

		g.setFont(font)
		val fm = g.getFontMetrics()
		val tw = fm.charsWidth(text.toCharArray, 0, text.length)
		val th = fm.getHeight()
		
		val tx = getTX(x, w, tw)
		val ty = getTY(y, h, fm)
		val cx = fm.charsWidth(text.toCharArray, 0, caret.col)

		if (selPos != caret) { 
			val spx = fm.charsWidth(text.toCharArray, 0, selPos.col)
			g.setPaint(new Color(0xDDDDDD))
			g.fillRect(tx + math.min(cx, spx), ty - th, math.abs(cx - spx), (3 * th) / 2)
		}

		g.setPaint(textColor)
		g.drawString(text, tx, ty)
		
		g.setPaint(if (Colors.dist(_bgColor, Color.WHITE) < 0.1) Color.BLACK else Color.WHITE)
		g.drawLine(tx + cx, ty - th, tx + cx, ty + th / 2)

		g.setClip(null)
	}
}
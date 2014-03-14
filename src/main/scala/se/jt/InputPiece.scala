package se.jt

import java.awt.Graphics2D
import java.awt.Color
import java.awt.TextArea
import java.awt.event._

import se.jt.frame.Piece
import se.jt.frame.UserInput
import se.jt.frame.Compass

import se.jt.event.Publisher
import se.jt.event.TypingDone
import se.jt.event.MouseResponsive

import scala.collection.mutable.Stack

object InputPiece {
	
	def apply(name:String) = new InputPiece(name)
}

class InputPiece(val name:String) extends Piece with Publisher with MouseResponsive {
	
	var caret = 0
	var selPos = 0
	
	override def select() = {
		selected = true
		makeDirty
	}
	
	val sb = new StringBuilder
	
	def textSelected = selPos != caret
	def deleteSelection = {
		sb.delete(math.min(selPos, caret), math.max(selPos, caret))
		caret = math.min(selPos, caret)
		selPos = caret
	}
	
	import UserInput._
	captures += {
		case PRINTABLECHAR(c, mods) =>
			if (textSelected) 
				deleteSelection
			
			sb.insert(caret, c)
			caret += 1
			selPos = caret
			makeDirty
			Eat
		
		case BACKSPACE() => 
			if (sb.nonEmpty) {
				if (textSelected) 
					deleteSelection
				else {
					selPos = caret - 1
					deleteSelection
				}
				makeDirty
			}
			Eat
		
		case ARROW(d, mods) =>
			import Compass._
			d match {
				case WEST => caret = math.max(0, caret-1)
				case EAST => caret = math.min(sb.length, caret+1)
				case _ => {}
			}
			if (!mods.shift) selPos = caret
			makeDirty
			Eat
		
		case ENTER() =>
			/// will focus move upon typing complete?
			publish(TypingDone(this))
			println("TYping done!")
			Eat
	}
	
	
	def rerender(g:Graphics2D):Unit = {
		g.setPaint(Color.LIGHT_GRAY)
		g.fillRect(x, y, w, h)
		g.clipRect(x, y, w, h)
		
		if (selected) {
			g.setPaint(Color.WHITE)
			g.drawRect(x,y,w-1,h-1)
		} else if (active) {
			g.setPaint(Color.RED)
			g.drawRect(x,y,w-1,h-1)
		}
		
		val text = sb.result
		
		val fm = g.getFontMetrics()
		val tw = fm.charsWidth(text.toCharArray, 0, text.length)
		val th = fm.getHeight()
		val tx = if (tw > w) x + w - (tw+5) else x + 5
		val ty = y + h/2 + fm.getAscent - th/2
		val cx = fm.charsWidth(text.toCharArray, 0, caret)
		
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
}
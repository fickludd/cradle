package se.jt

import se.jt.frame.Piece
import se.jt.frame.Configurable
import se.jt.frame.Scrollable
import se.jt.frame.UserPermission
import se.jt.frame.Colors
import se.jt.text.Typeable
import se.jt.text.TextPos
import se.jt.text.Fonts
import se.jt.event.PreferredSizeChanged
import se.jt.input.MouseResponsive
import se.jt.input.UserInput

import java.awt.Color
import java.awt.Graphics2D


object EditorPiece {
	def apply(name:String) = new EditorPiece(name)
	def apply(name:String, text:String) = {
		val ep = new EditorPiece(name)
		ep.text(text)
		ep
	}
	
	class EditorPieceBase(
			val name:String
	) extends Scrollable 
		with Typeable with Configurable.Text {
		
		var _lines:List[StringBuilder] = List(new StringBuilder)
		var charFilter:Char => Boolean = c => true
		
		def text(str:String) = {
			_lines = str.split("\n").map(line => new StringBuilder(line)).toList
			publish(PreferredSizeChanged(this))
			makeDirty
		}
		override def defaultPH = 
			Fonts.getFontMetrics(font).getHeight * _lines.length
		
		
		def deleteSelection:Unit = {
			val (p1, p2) = if (caret < selPos) (caret, selPos) else (selPos, caret)
			if (p1.row == p2.row) {
				_lines(p1.row).delete(p1.col, p2.col)
			} else {
				_lines(p1.row).delete(p1.col, _lines(p1.row).length())
				_lines(p1.row) ++= _lines(p2.row).drop(p2.col)
				_lines = _lines.take(p1.row + 1) ::: _lines.takeRight(_lines.length - p2.row - 1)
			}
			selPos = p1
			caret = p1
		}
		def insert(pos:TextPos, text:String):Unit =
			_lines(pos.row).insert(pos.col, text) 
		def insert(pos:TextPos, c:Char):Unit =
			c match {
				case '\n' => 
					val front = _lines.take(pos.row)
					val parts = _lines(pos.row).splitAt(pos.col)
					val back = _lines.drop(pos.row + 1)
					_lines = front ::: (parts._1 :: parts._2 :: back)
				case c => _lines(pos.row).insert(pos.col, c)
			}
		def moveRow(pos:TextPos, n:Int):TextPos = {
			val nRow = math.min(_lines.length, math.max(0, pos.row + n))
			TextPos(nRow, math.min(_lines(nRow).length, pos.col))
		}
		def movePos(pos:TextPos, n:Int):TextPos =
			if (pos.col + n > _lines(pos.row).length)
				if (pos.row + 1 == _lines.length)
					TextPos(pos.row, _lines(pos.row).length)
				else
					movePos(TextPos(pos.row+1, 0), n - (_lines(pos.row).length - pos.col) - 1)
			else if (pos.col + n < 0)
				if (pos.row == 0)
					TextPos(0,0)
				else
					movePos(TextPos(pos.row-1, _lines(pos.row-1).length), n + pos.col + 1)
			else TextPos(pos.row, pos.col + n)
		def hasText:Boolean = _lines.head.nonEmpty
		def toTextPos(x:Int, y:Int):TextPos = {
			if (!hasText) return TextPos(0,0)
			val fm = Fonts.getFontMetrics(font)
			val lh = fm.getHeight
			val contRect = contentRect
			val line = math.min(_lines.length-1, (y - contRect.y + vy) / lh)
			val charDel = Fonts.toCharDel(x - contRect.x + vx, fm, _lines(line).result())
			TextPos(line, charDel)
		}
		
		import UserInput._
		import UserPermission._
		captures += {
			case ENTER() =>
				/// will focus move upon typing complete?
				captures(PRINTABLECHAR('\n', Mods(false, false, false)))
				Eat
		}
		
		def rerender(g:Graphics2D, x:Int, y:Int, w:Int, h:Int):Unit = {
			if (!hasText) return
			
			g.setFont(font)
			val fm = g.getFontMetrics(font)
			val fontHeight = fm.getHeight()
			
			val (cw, ch) = visibleSize
			
			g.setPaint(new Color(0xDDDDDD))
			if (selPos != caret) {
				val (p1, p2) = if (caret < selPos) (caret, selPos) else (selPos, caret)
			
				val startStops = 
					_lines.zipWithIndex.slice(p1.row, p2.row+1)
						.map(t => Array(0, fm.stringWidth(t._1.result), t._2))
				if (startStops.nonEmpty) {
					startStops.head(0) = fm.charsWidth(_lines(startStops.head(2)).toArray, 0, p1.col)
					startStops.last(1) = fm.charsWidth(_lines(startStops.last(2)).toArray, 0, p2.col)
				}
				
				for (ss <- startStops) {
					val chArr = _lines(ss(2)).toArray
					g.fillRect(x - vx + ss(0), y - vy + fontHeight*ss(2), ss(1) - ss(0), fontHeight)
				}
			}
			
			val caretx = fm.charsWidth(_lines(caret.row).result.toCharArray, 0, caret.col)
			g.setPaint(if (Colors.dist(_bgColor, Color.WHITE) < 0.1) Color.BLACK else Color.WHITE)
			g.drawLine(
					x - vx + caretx, y - vy + fontHeight*caret.row, 
					x - vx + caretx, y - vy + fontHeight*(caret.row+1))
			
			g.setPaint(textColor)
			var ty = 0
			for (i <- 0 until _lines.length) {
				if (ty < vy + vh && ty + fontHeight > vy)
					g.drawString(
							_lines(i).result, 
							x + fm.getLeading - vx, 
							y + ty + fm.getAscent - vy)
				ty += fontHeight
			}
		}
	}
	
}

class EditorPiece(
		name:String
) extends ScrollPiece(name, new EditorPiece.EditorPieceBase("base")) {
	
	def text(str:String) = piece.text(str)
	
	override def bgColor = None
	
	override def configure(vals:Seq[(String, String)]):Piece = piece.configure(vals)
}
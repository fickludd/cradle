package se.jt

import se.jt.frame.Scrollable
import se.jt.text.Typeset
import se.jt.text.Typeset.Typesetting
import se.jt.text.Typeset.TexTypesetting
import se.jt.text.SelectableText
import se.jt.text.TextPos
import se.jt.input.MouseResponsive
import se.jt.frame.Colors
import se.jt.frame.Configurable
import se.jt.frame.Piece
import se.jt.text.Fonts
import se.jt.frame.Geom.Frame
import se.jt.event.PreferredSizeChanged
import scala.util.Random
import java.awt.Color
import java.awt.Graphics2D
import java.awt.BasicStroke
import java.awt.FontMetrics
import se.jt.text.Fonts

object ReadPiece {
	def apply(name:String) = new ReadPiece(name)
	def apply(name:String, text:String) = {
		val tp = new ReadPiece(name)
		tp.text(text)
		tp
	}
	
	class ReadPieceBase(
			val name:String
	) extends Scrollable  
		with SelectableText with Configurable.Text {
		
		import se.jt.frame.Compass._
		
		configs += (
				("typesetter", _.toLowerCase match {
					case "manual" 	=> setTypesetter(Typeset.Manual(_,_,_))
					case "tex" 		=> setTypesetter(TexTypesetting(_,_,_))
				})
			)
		
		private var _typeset:(String, FontMetrics, Int) => Typesetting = Typeset.Manual(_,_,_)
		def setTypesetter(typesetter:(String, FontMetrics, Int) => Typesetting) = {
			_typeset = typesetter
			text(_text.raw)
		}
		
		
		var charFilter:Char => Boolean = c => true
		var _text:Typesetting = Typeset.Manual("", Fonts.getFontMetrics(font), 100)
		_padding = Frame(2)
		
		
		
		def moveRow(pos:TextPos, n:Int):TextPos = {
			val nRow = math.min(_text.lines.length, math.max(0, pos.row + n))
			TextPos(nRow, math.min(_text.lines(nRow).length, pos.col))
		}
		def movePos(pos:TextPos, n:Int):TextPos =
			if (pos.col + n > _text.lines(pos.row).length)
				movePos(TextPos(pos.row+1, 0), n - _text.lines(pos.row).length + pos.col)
			else TextPos(pos.row, pos.col + n)
		def hasText:Boolean = _text.raw.length > 0
		def toTextPos(x:Int, y:Int):TextPos = {
			if (!hasText) return TextPos(0,0)
			val fm = Fonts.getFontMetrics(font)
			val lh = fm.getHeight
			val contRect = contentRect
			val line = math.min(_text.lines.length - 1, (y - contRect.y + vy) / lh)
			val charDel = Fonts.toCharDel(x - contRect.x + vx, fm, _text.lines(line))
			TextPos(line, charDel)
		}
		
		
		override def defaultPH = _text.h
		override def size_=(t: (Int, Int)) = {
			super.size_=(t)
			_text = _text.withWidth(t._1)
			makeDirty
		}
		override def setViewPortSize(vw:Int, vh:Int) = {
			super.setViewPortSize(vw, vh)
			_text = _text.withWidth(vw)
			makeDirty
		}
		
		def text(str:String) = {
			_text = _typeset(str, Fonts.getFontMetrics(font), w)
			publish(PreferredSizeChanged(this))
			makeDirty
		}
		
		def rerender(g:Graphics2D, x:Int, y:Int, w:Int, h:Int):Unit = {
			if (!hasText) return
			
			g.setFont(font)
			val fm = g.getFontMetrics(font)
			val fontHeight = fm.getHeight()
			
			val (cw, ch) = visibleSize
			
			g.setPaint(new Color(0xDDDDDD))
			if (selPos != caret) {
				
				val startStops = _text.lines.zipWithIndex.slice(
										math.min(selPos.row, caret.row), 
										math.max(selPos.row, caret.row)+1
								).map(t => Array(0, fm.stringWidth(t._1), t._2))
				if (startStops.nonEmpty) {
					startStops.head(0) = fm.charsWidth(_text.lines(startStops.head(2)).toArray, 0, 
							if (selPos.row <= caret.row) selPos.col else caret.col)
					startStops.last(1) = fm.charsWidth(_text.lines(startStops.last(2)).toArray, 0, 
							if (selPos.row <= caret.row) caret.col else selPos.col)
				}
				
				for (ss <- startStops) {
					val chArr = _text.lines(ss(2)).toArray
					g.fillRect(x - vx + ss(0), y - vy + fontHeight*ss(2), ss(1) - ss(0), fontHeight)
				}
			}
			
			
			
			g.setPaint(textColor)
			var ty = 0
			for (i <- 0 until _text.lines.length) {
				if (ty < vy + vh && ty + fontHeight > vy)
					g.drawString(
							_text.lines(i), 
							x + fm.getLeading - vx, 
							y + ty + fm.getAscent - vy)
				ty += fontHeight
			}
		}
	}
}

class ReadPiece(
		name:String
) extends ScrollPiece(name, new ReadPiece.ReadPieceBase("base")) 
	with Configurable.Text {
	
	def text(str:String) = piece.text(str)
	
	override def bgColor = None
	
	override def configure(vals:Seq[(String, String)]):Piece = piece.configure(vals)
} 

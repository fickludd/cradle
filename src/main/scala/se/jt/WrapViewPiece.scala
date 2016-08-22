package se.jt

import java.awt.Color
import java.awt.Graphics2D

import se.jt.frame.Piece
import se.jt.frame.PurePoserPiece
import se.jt.frame.Configurable

object WrapViewPiece {
	def apply(name:String) = new WrapViewPiece(name)
}

class WrapViewPiece(
		val name:String
) extends PurePoserPiece with Configurable {
	
	renderBefore = true
	
	var _items:Seq[Piece] = Nil
	def items_=(items:Seq[Piece]):Unit = {
		_items 	= items
		makeDirty
	}
	
	def pieces = _items.map(p => p.name -> p).toMap
	
	def framedRepose(x:Int, y:Int, w:Int, h:Int):Unit = {
		def layoutCol(colx:Int, col:Seq[Piece]):Int = {
			var ty = 0
			for (p <- col) {
				p.pos = (x + colx, y + ty)
				ty += p.h
			}
			colx + col.map(_.w).max
		}
		def layout(colx:Int, ps:Seq[Piece], col:List[Piece]):Unit = {
			ps match {
				case Nil => 
					if (col.nonEmpty)
						layoutCol(colx, col.reverse)
				case p::rest =>
					if (col.isEmpty || (col.map(_.h).sum + p.h < h)) 
						layout(colx, rest, p::col)
					else {
						layout(layoutCol(colx, col.reverse), rest, p::Nil)
					}
			}
		}
		_items.foreach(p => p.size = (p.pw, p.ph))
		layout(0, _items, Nil)
	}
}
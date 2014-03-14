package se.jt

import se.jt.frame.ScrollablePoser
import se.jt.frame.Piece
import java.awt.Graphics2D

object GridPiece {
	
	def apply(name:String, rows:Seq[Piece]*) = new GridPiece(name, rows)
}

class GridPiece(val name:String, val rows:Seq[Seq[Piece]]) extends ScrollablePoser {

	var rowMinHeight = 30
	ph = rows.length * rowMinHeight
	
	lazy val pieces = rows.flatten.map(p => p.name -> p).toMap
	
	def rerender(g:Graphics2D):Unit = {}
	/*
	override def renderTree(g:Graphics2D, t:Long):Unit = {
		if (renderBefore)
			rerender(g)
		g.setClip(x, y, w, h)
		for (p <- pieces.values)
			p match {
				case pos:Poser => pos.renderTree(g, t)
				case _ => p.render(g, t)
			}
		g.setClip(null)
		if (!renderBefore)
			rerender(g)
	}
	*/
	def repose() = {
		val nCols = rows.map(_.length).max
		
		val (dw, dh) = visibleSize
		val cw = dw / nCols
		var temp_y = 0
		for (row <- rows) {
			var temp_x = 0
			for (p <- row) {
				if (!viewPortActive || (temp_y < vy + dh && temp_y + rowMinHeight > vy)) {
					p.pos = (x + temp_x - vx, y + temp_y - vy)
					p.size = (cw, rowMinHeight)
				}
				temp_x += cw
			}
			temp_y += rowMinHeight
		}
	}
	
}
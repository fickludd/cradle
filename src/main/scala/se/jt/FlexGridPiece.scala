package se.jt

import se.jt.frame.ScrollablePoser
import se.jt.frame.Piece
import se.jt.frame.Poser
import java.awt.Graphics2D
import se.jt.frame.Configurable
import se.jt.frame.Geom.Rect

object FlexGridPiece {
  
	def apply(name:String, parts:(Piece, Rect)*) = new FlexGridPiece(name, parts)
}

class FlexGridPiece(
		val name:String, 
		val parts:Seq[(Piece, Rect)]
) extends ScrollablePoser with Configurable {

	configs += (
		("kerning", x => kerning = x.toInt)
	)
	
	var kerning = 0
	val nRows = parts.map(t => t._2.y + t._2.h).max
	val nCols = parts.map(t => t._2.x + t._2.w).max
	override def defaultPH = nRows * 30
	
	lazy val pieces = parts.map(t => t._1.name -> t._1).toMap
	
	//override def bgColor = None
	
	/*
	override def renderTree(g:Graphics2D, t:Long):Unit = {
		if (renderBefore)
			rerender(g, x, y, w, h)
		g.setClip(x, y, w, h)
		for (p <- pieces.values)
			p match {
				case pos:Poser => pos.renderTree(g, t)
				case _ => p.render(g, t)
			}
		g.setClip(null)
		if (!renderBefore)
			rerender(g, x, y, w, h)
	}
	*/
	def framedRepose(x:Int, y:Int, w:Int, h:Int) = {
		val (dw, dh) = visibleSize
		val ch = dh / nRows
		val cw = dw / nCols
		for ((p,ir) <- parts) {
			if (!viewPortActive || (
					cw * ir.x < vx + dw 	&& 
					cw * (ir.x + ir.w) > vx && 
					ch * ir.y < vy + dh 	&& 
					ch * (ir.y + ir.h) > vy)
			) {
				p.pos = (x + cw * ir.x + kerning - vx, y + ch * ir.y + kerning - vy)
				p.size = (cw * ir.w - kerning*2, ch * ir.h - kerning*2)
			}
		}
	}
	
}
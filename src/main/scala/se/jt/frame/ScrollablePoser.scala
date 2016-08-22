package se.jt.frame

import java.awt.Graphics2D
import java.awt.Color

trait ScrollablePoser extends Scrollable with PurePoserPiece {
	
	override def renderTree(g:Graphics2D, underlyingColor:Color, t:Long):Unit = {
		//if (renderBefore)
		//	framedRerender(g)
		if (isDirty) {
			isDirty = false
			renderFrame(g, bgColor.getOrElse(underlyingColor))
		}
		
		val (dw, dh) 	= visibleSize
		val visRect 	= Geom.Rect(x, y, dw, dh)
		//g.setClip(x, y, w, h)
		for (p <- pieces.values) {
			if (p.overlaps(visRect))
				p match {
					case pos:Poser => pos.renderTree(g, bgColor.getOrElse(underlyingColor), t)
					case _ => p.render(g, bgColor.getOrElse(underlyingColor), t)
				}
		}
		//g.setClip(null)
		//if (!renderBefore)
		//	framedRerender(g)
	}
}
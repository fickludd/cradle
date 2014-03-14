package se.jt.frame

import java.awt.Graphics2D

trait ScrollablePoser extends Scrollable with PoserPiece {
	
	override def renderTree(g:Graphics2D, t:Long):Unit = {
		if (renderBefore)
			rerender(g)
		
		val (dw, dh) = visibleSize
		//g.setClip(x, y, w, h)
		for (p <- pieces.values) {
			if (p.overlaps(Geom.Rect(x, y, dw, dh)))
				p match {
					case pos:Poser => pos.renderTree(g, t)
					case _ => p.render(g, t)
				}
		}
		//g.setClip(null)
		if (!renderBefore)
			rerender(g)
	}
}
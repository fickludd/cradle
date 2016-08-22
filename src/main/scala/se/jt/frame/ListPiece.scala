package se.jt.frame

import java.awt.Graphics2D
import java.awt.Color

import se.jt.event.PreferredSizeChanged

trait ListPiece[T] extends Scrollable {
	
	protected var selPos = 0
	val rowHeight = 20
	var _items:Seq[T] = Nil
	def items_=(items:Seq[T]):Unit = {
		_items 	= items
		selPos 	= 0
		publish(PreferredSizeChanged(this))
		makeDirty
	}
	
	override def defaultPH = rowHeight * _items.length 
	
	def selUp = {
		selPos = math.max(0, selPos-1)
		makeDirty
	}
	def selDown = {
		selPos = math.min(_items.length, selPos+1)
		makeDirty
	}
	def selIndex_=(i:Int) = 
		if (i != selPos) {
			selPos = math.max(0, math.min(_items.length, i))
			makeDirty
		}
	def hasSelected = selPos > 0
	def getSelected = _items(selPos-1)
	
	def itemHeight(t:T, g:Graphics2D):Int
	def renderItem(t:T, g:Graphics2D, x:Int, y:Int, selected:Boolean, odd:Boolean):Unit
	
	//override def preferredSize = Some(w, _items.length * rowHeight)
	
	def rerender(g:Graphics2D, x:Int, y:Int, w:Int, h:Int):Unit = {
		g.setPaint(Color.DARK_GRAY)
		if (viewPortActive)
			g.fillRect(x, y, vw, vh)
		else
			g.fillRect(x, y, w, h)
		
		var hy = 0
		for (i <- 0 until _items.length) {
			val th = itemHeight(_items(i), g)
			if (!viewPortActive || (hy < vy + vh && hy + th > vy)) {
				renderItem(_items(i), g, x - vx, y + hy - vy, i+1 == selPos, i%2 == 1)
			}
			hy += itemHeight(_items(i), g)
		}
	}
}
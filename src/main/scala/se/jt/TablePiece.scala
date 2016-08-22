package se.jt

import java.awt.Graphics2D
import java.awt.Color
import se.jt.frame.ListPiece
import se.jt.frame.Configurable

class TablePiece(
		val name:String, 
		data:Array[Array[Any]], 
		var columns:Seq[String]
) extends ListPiece[Array[Any]] with Configurable {
	
	items_=(data)
	
	var colWidths:Option[Seq[Int]] = None
	var drawColWidths:Seq[Int] = Nil
	var cellBG = new Color(0xBAFCDE)
	
	def itemHeight(t:Array[Any], g:Graphics2D):Int = {
		val fm = g.getFontMetrics()
		fm.getHeight()
	}
	
	def renderHeader(col:String, g:Graphics2D, x:Int, y:Int, w:Int) = {
		val fm = g.getFontMetrics()
		val h = fm.getHeight()
		g.setPaint(Color.BLACK)
		g.fillRect(x, y, w, h)
		g.setPaint(Color.WHITE)
		g.drawString(col, x + 5, y + fm.getAscent)
	}
	
	def renderCell(c:String, g:Graphics2D, x:Int, y:Int, w:Int) = {
		val fm = g.getFontMetrics()
		val h = fm.getHeight()
		g.setPaint(cellBG)
		g.fillRect(x, y, w, h)
		g.setPaint(Color.BLACK)
		g.drawString(c, x + 5, y + fm.getAscent)
	}
	
	override def rerender(g:Graphics2D, x:Int, y:Int, w:Int, h:Int):Unit = {
		val fm = g.getFontMetrics()
		if (colWidths.isEmpty) {
			val headWidths = columns.map(fm.stringWidth(_) + 20)
			val cellWidths = _items.take(100).map(
								_.map(a => fm.stringWidth(a.toString)+5)
							).transpose.map(_.max)
			colWidths = Some(headWidths.zip(cellWidths).map(t => math.max(t._1, t._2)))
		}
		val headerHeight = fm.getHeight
		
		val (dw, dh) = visibleSize
		g.setPaint(cellBG)
		g.fillRect(x, y, dw, dh)
		
		val cwsum = colWidths.get.sum.toDouble
		drawColWidths = 
			if (cwsum < dw) 
				colWidths.get.map(cw => ((cw / cwsum) * dw).toInt)
			else
				colWidths.get
			
		var hy = 0
		for (i <- 0 until _items.length) {
			val th = itemHeight(_items(i), g)
			if (hy < vy + dh - headerHeight && hy + th >= vy) {
				renderItem(_items(i), g, x - vx, y + hy + headerHeight - vy, i+1 == selPos, i%2 == 1)
			}
			hy += th
		}
		
		var curw = 0
		for ((c, colw) <- columns zip drawColWidths) {
			if (!viewPortActive || (curw < vx + vw && curw + colw > vx)) {
				renderHeader(c, g, x + curw - vx, y - vy, colw)
			}
			curw += colw
		}
	}
	
	def renderItem(
			a:Array[Any], 
			g:Graphics2D, 
			x:Int, 
			y:Int,
			selected:Boolean,
			odd:Boolean
	):Unit = {
		//val rh = itemHeight(a, g)
		
		var curw = 0
		for ((cell, colw) <- a zip drawColWidths) {
			if (!viewPortActive || (curw < vx + vw && curw + colw > vx)) {
				renderCell(cell.toString, g, x + curw - vx, y - vy, colw)
			}
			curw += colw
		}
	}
}
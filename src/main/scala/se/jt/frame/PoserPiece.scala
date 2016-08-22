package se.jt.frame

import java.awt.Color
import java.awt.Graphics2D

trait PoserPiece extends Poser with Piece//KvimTreeNode[String, String]  
{
	
	var renderBefore = true
	
	/*override def setPieces(ps:Seq[Piece]) = {
		pieces.clear
		for (p <- ps) pieces += p
		makeDirty
	}*/
	
	def framedRepose(x:Int, y:Int, w:Int, h:Int)
	def repose():Unit = {
		val cr = contentRect
		framedRepose(cr.x, cr.y, cr.w, cr.h)
	}
		
	
	override def apply(path:TreePath.Path):Piece = 
		path match {
			case Nil => this
			case k::ks => pieces(k)(ks)
		}
	
	override def realize(path:TreePath.Path):List[Piece] = 
		path match {
			case Nil => List(this)
			case k::ks => this :: pieces(k).realize(ks)
		}
	
	override def makeDirty = {
		isDirty = true
		needRepose = true
	}
	
	override def renderTree(g:Graphics2D, underlyingColor:Color, t:Long):Unit = {
		if (renderBefore) render(g, bgColor.getOrElse(underlyingColor), t)
		for (p <- pieces.values)
			p match {
				case pos:Poser => pos.renderTree(g, bgColor.getOrElse(underlyingColor), t)
				case _ => p.render(g, bgColor.getOrElse(underlyingColor), t)
			}
		if (!renderBefore) render(g, bgColor.getOrElse(underlyingColor), t)
	}
	
	override def cradleTree(c:se.jt.Cradle):Unit = {
		cradle(c)
		for (p <- pieces.values) 
			p match {
				case pos:Poser => pos.cradleTree(c)
				case _ => p.cradle(c)
			}
	}
}
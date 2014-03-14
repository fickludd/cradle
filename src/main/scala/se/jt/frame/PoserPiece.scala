package se.jt.frame

import java.awt.Graphics2D

trait PoserPiece extends Poser with Piece//KvimTreeNode[String, String]  
{
	
	var renderBefore = true
	
	/*override def setPieces(ps:Seq[Piece]) = {
		pieces.clear
		for (p <- ps) pieces += p
		makeDirty
	}*/
	
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
	
	override def renderTree(g:Graphics2D, t:Long):Unit = {
		def doRerender = {
			rerender(g)
			isDirty = false
		}
		if (renderBefore && isDirty) doRerender
		for (p <- pieces.values)
			p match {
				case pos:Poser => pos.renderTree(g, t)
				case _ => p.render(g, t)
			}
		if (!renderBefore) doRerender
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
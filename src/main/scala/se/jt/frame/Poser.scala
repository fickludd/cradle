package se.jt.frame

import java.awt.Graphics2D
import scala.collection.mutable.ArrayBuffer




trait Poser {

	import Geom._
	import TreePath._
	
	var needRepose:Boolean = true
	
	def repose():Unit
	def posePieces():Unit = {
		if (needRepose) {
			repose()
			needRepose = false
		}
		(pieces.values.collect { case p:Poser => p }).foreach(_.posePieces) 
	}
	/*def setPieces(ps:Seq[Piece]) = {
		pieces.clear
		for (p <- ps) pieces += p
		needRepose = true
	}*/
	def pieces:Map[String, Piece]
	def apply(path:Path):Piece = path match {
		case Nil => throw new Exception
		case k::ks => pieces(k)(ks)
	}
	
	def renderTree(g:Graphics2D, t:Long):Unit = 
		for (p <- pieces.values) 
			p match {
				case pos:Poser => pos.renderTree(g, t)
				case _ => p.render(g, t)
			}
	
	def getPathAt(x:Int, y:Int):Path = {
		pieces.flatMap(t => {
			val (k, p) = t
			p match {
				case pp:PoserPiece =>
					(
							if (pp.wraps(x, y)) List(k)
							else Nil
					) ++ pp.getPathAt(x, y)
				case p:Piece => 
					if (p.wraps(x, y)) List(k)
					else Nil
			}		
		}).toSeq
	}
	
	def getTouchingPieces(r:Rect):Seq[Piece] = {
		pieces.flatMap(t => {
				val (k, p) = t
				p match {
				case pp:PoserPiece =>
					pp.overlapsOrNil(r) ++ pp.getTouchingPieces(r)
				case p:Piece => p.overlapsOrNil(r)
			}
		}).toSeq
	}
}
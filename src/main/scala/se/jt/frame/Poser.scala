package se.jt.frame

import java.awt.Graphics2D
import java.awt.Color
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
		(pieces.values.collect { case p:Poser => p }).foreach(
				_.posePieces()
			) 
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
	
	def renderTree(g:Graphics2D, underlyingColor:Color, t:Long):Unit = 
		for (p <- pieces.values) 
			p match {
				case pos:Poser => pos.renderTree(g, underlyingColor, t)
				case _ => p.render(g, underlyingColor, t)
			}
	
	def cradleTree(c:se.jt.Cradle):Unit = 
		for (p <- pieces.values) 
			p match {
				case pos:Poser => pos.cradleTree(c)
				case _ => p.cradle(c)
			}
	
	def getPathAt(x:Int, y:Int):Path = {
		pieces.find(_._2.wraps(x, y)) map (t => {
			val (k, p) = t
			p match {
				case pp:PoserPiece =>
					List(k) ++ pp.getPathAt(x, y)
				case p:Piece => List(k)
			}
		}) getOrElse Nil
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
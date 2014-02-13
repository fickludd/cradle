package se.jt

import java.awt.Graphics2D
import se.jt.frame.Piece
import se.jt.frame.PoserPiece

import se.jt.frame.Compass

object BorderPiece {
	
	def apply(name:String, children:(Compass.Dir, Piece)*) = 
		new BorderPiece(name, children.toMap)
	
}

class BorderPiece(
		val name:String, 
		val children:Map[Compass.Dir, Piece]
) extends PoserPiece {
	
	import Compass._
	
	def pieces = children.map(t => (t._1.name, t._2))
	/*
	def border(p:Piece) = {
		pieces += p
		makeDirty
		Some(p)
	}*/
	
	/*
	def north_=(p:Piece) 	= new BorderPiece(name, pieces + (NORTH -> p))
	def south_=(p:Piece) 	= new BorderPiece(name, pieces + (SOUTH -> p))
	def west_=(p:Piece) 	= new BorderPiece(name, pieces + (WEST -> p))
	def east_=(p:Piece) 	= new BorderPiece(name, pieces + (EAST -> p))
	def center_=(p:Piece)	= new BorderPiece(name, pieces + (CENTER -> p))
	*/
	
	def repose():Unit = {
		
		val ns = List(NORTH_WEST, NORTH, NORTH_EAST)
		val nh = if (ns.exists(children.contains(_))) {
			ns.filter(children.contains(_)).map(dir => {
				val p = children(dir)
				p.preferredSize match {
					case Some((pw, ph)) => ph
					case None 			=> h / 10
				}
			}).max
		} else 0
		
		val ss = List(SOUTH_WEST, SOUTH, SOUTH_EAST)
		val sh = if (ss.exists(children.contains(_))) {
			ss.filter(children.contains(_)).map(dir => {
				val p = children(dir)
				p.preferredSize match {
					case Some((pw, ph)) => ph
					case None 			=> h / 10
				}
			}).max
		} else 0
		
		val ws = List(SOUTH_WEST, WEST, NORTH_WEST)
		val ww = if (ws.exists(children.contains(_))) {
			ws.filter(children.contains(_)).map(dir => {
				val p = children(dir)
				p.preferredSize match {
					case Some((pw, ph)) => pw
					case None 			=> w / 10
				}
			}).max
		} else 0
		
		val es = List(SOUTH_EAST, EAST, NORTH_EAST)
		val ew = if (es.exists(children.contains(_))) {
			es.filter(children.contains(_)).map(dir => {
				val p = children(dir)
				p.preferredSize match {
					case Some((pw, ph)) => pw
					case None 			=> w / 10
				}
			}).max
		} else 0
		
		def position(dir:Dir, x:Int, y:Int, w:Int, h:Int) = 
			for (p <- children.get(dir)) {
				p.pos = (x, y)
				p.size = (w, h)
			}
		
		def ifDir(dir:Dir, a:Int) = 
			if (children.contains(dir)) a else 0
		
		position(NORTH_WEST, x, y, ww, nh)
		position(NORTH_EAST, x + w - ew, y, ew, nh)
		position(SOUTH_WEST, x, y + h - sh, ww, sh)
		position(SOUTH_EAST, x + w - ew, y + h - sh, ew, sh)
		
		position(NORTH, x + ifDir(NORTH_WEST, ww), y, 
						w - ifDir(NORTH_WEST, ww) - ifDir(NORTH_EAST, ew), nh)
		position(SOUTH, x + ifDir(SOUTH_WEST, ww), y + h - sh, 
						w - ifDir(SOUTH_WEST, ww) - ifDir(SOUTH_EAST, ew), sh)
		position(WEST, x, y + nh, ww, h - nh - sh)
		position(EAST, x + w - ew, y + nh, ew, h - nh - sh)
		
		position(CENTER, x+ww, y+nh, w-ww-ew, h-nh-sh)
	}
	
	def rerender(g:Graphics2D):Unit = {}
}
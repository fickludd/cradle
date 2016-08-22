package se.jt.frame

import java.awt.Graphics2D

trait PurePoserPiece extends PoserPiece {

	def rerender(g:Graphics2D, x:Int, y:Int, w:Int, h:Int):Unit = {}
}
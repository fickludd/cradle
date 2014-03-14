package se.jt

import java.awt.Graphics2D

import se.jt.frame.Piece
import se.jt.frame.PoserPiece
import se.jt.event.InputDone

object TabPiece {
	def apply(name:String, children:Piece*) =
		new TabPiece(name, children)
}

class TabPiece(
		val name:String,
		val children:Seq[Piece]
) extends PoserPiece {
	
	val tabRowHeight = 20
	val tabRow = new EastWestStack("tabRow", 
			children.map(p => ButtonPiece(p.name, () => showTab(p.name))))
	
	
	var active = children.head
	def pieces = Map(active.name -> active, "tabRow" -> tabRow)
	
	def repose():Unit = {
		active.pos = (x, y + tabRowHeight)
		active.size = (w, h - tabRowHeight)
		tabRow.pos = (x, y)
		tabRow.size = (w, tabRowHeight)
	}
	
	def tabNames = children.map(_.name)
	def showTab(tabName:String) = {
		children.find(_.name == tabName) match {
			case Some(t) =>
				active = t
				makeDirty
			case None =>
				throw new Exception("No tab with name '"+tabName+ "'")
		}
	}
	
	def rerender(g:Graphics2D):Unit = {}
}
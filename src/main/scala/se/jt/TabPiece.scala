package se.jt

import java.awt.Graphics2D

import se.jt.frame.Piece
import se.jt.frame.PurePoserPiece
import se.jt.frame.Configurable
import se.jt.event.InputDone
import se.jt.event.Reactor
import se.jt.event.Selection

object TabPiece {
	def apply(name:String, children:Piece*) =
		new TabPiece(name, children)
}

class TabPiece(
		val name:String,
		val children:Seq[Piece]
) extends PurePoserPiece with Reactor with Configurable {
	
	val tabRowHeight = 20
	val tabRow = new EastWestOneOffPiece("tabRow", 
			children.map(_.name))
	
	listenTo(tabRow)
	reactions += {
		case Selection(p, s:String) =>
			showTab(s)
	}
	
	var active = children.head
	def pieces = Map(active.name -> active, "tabRow" -> tabRow)
	
	def framedRepose(x:Int, y:Int, w:Int, h:Int):Unit = {
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
}
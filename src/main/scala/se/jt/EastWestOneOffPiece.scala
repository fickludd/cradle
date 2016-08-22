package se.jt

import java.awt.Color
import java.awt.Graphics2D

import se.jt.frame.PurePoserPiece
import se.jt.frame.Piece
import se.jt.frame.Configurable
import se.jt.event.Publisher
import se.jt.event.Selection

object EastWestOneOffPiece {
	def apply(name:String, buttons:String*) =
		new EastWestOneOffPiece(name, buttons)
}

class EastWestOneOffPiece(
		val name:String,
		val buttons:Seq[String]
) extends PurePoserPiece with Publisher with Configurable {
	
	val _btns:Seq[ToggleButtonPiece] =
		buttons.map(s => ToggleButtonPiece(s, () => {
			_btns.filter(_.name != s).foreach(_.up())
			_btns.find(_.name == s).get.down()
			publish(Selection(this, s))
		}))
	
	_btns.head.down()
		
	val btnRow = new EastWestStack("btnRow", _btns)
	
	def framedRepose(x:Int, y:Int, w:Int, h:Int):Unit = {
		btnRow.pos = (x, y)
		btnRow.size = (w, h)
	}
	
	def pieces = Map("buttonRow" -> btnRow)
}
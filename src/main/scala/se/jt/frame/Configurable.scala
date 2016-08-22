package se.jt.frame

import scala.collection.mutable.HashMap
import java.awt.Color
import se.jt.frame.Compass._
import se.jt.frame.Geom.Frame
import se.jt.text.Fonts

trait Configurable extends Piece {

	import Configurable._
	protected val configs = new HashMap[String, String => Unit]
	
	configs += (
		("width", x => userPrefWidth(parseInt(x))),
		("height", x => userPrefHeight(parseInt(x))),
		("bgColor", x => _bgColor = new Color(parseInt(x))),
		("borderColor", x => _borderColor = new Color(parseInt(x))),
		
		("border", x => _border = Frame(parseInt(x))),
		("borderEast", 	x => _border = _border.withEast(parseInt(x))),
		("borderWest", 	x => _border = _border.withWest(parseInt(x))),
		("borderNorth", x => _border = _border.withNorth(parseInt(x))),
		("borderSouth", x => _border = _border.withSouth(parseInt(x))),
		
		("margin", x => _margin = Frame(parseInt(x))),
		("marginEast", 	x => _margin = _margin.withEast(parseInt(x))),
		("marginWest", 	x => _margin = _margin.withWest(parseInt(x))),
		("marginNorth", x => _margin = _margin.withNorth(parseInt(x))),
		("marginSouth", x => _margin = _margin.withSouth(parseInt(x))),
		
		("padding", x => _padding = Frame(parseInt(x))),
		("paddingEast", 	x => _padding = _padding.withEast(parseInt(x))),
		("paddingWest", 	x => _padding = _padding.withWest(parseInt(x))),
		("paddingNorth", 	x => _padding = _padding.withNorth(parseInt(x))),
		("paddingSouth", 	x => _padding = _padding.withSouth(parseInt(x)))
	)
	
	var _bgColor 		= Color.LIGHT_GRAY
	var _borderColor 	= Color.DARK_GRAY
	
	def bgColor:Option[Color] = Some(_bgColor)
	def borderColor:Option[Color] = Some(_borderColor)
	
	def configure(vals:Seq[(String, String)]):Piece = {
		var dirties = false
		for ((k, v) <- vals) {
			if (configs contains k) {
				try {
					configs(k)(v)
					dirties = true
				} catch {
					case e:Exception =>
						println("error in configuring '"+k+"': "+e)
				}
			}
		}
		if (dirties)
			makeDirty
		this
	}
	
}

object Configurable {
	
	def parseInt(str:String):Int =
		if (str.take(2) == "0x")
			Integer.parseInt(str.drop(2), 16)
		else if (str.takeRight(2) == "px")
			str.dropRight(2).toInt
		else str.toInt
	
	trait Text extends Configurable {
			
			configs += (
				("textColor", x => textColor = new Color(parseInt(x))),
				("font", x => font = Fonts.getFont(x)),
				("valign", x => northSouthAlign = dir(x)),
				("halign", x => eastWestAlign = dir(x)),
				("kerning", x => kerning = x.toInt)
			)
		
			var northSouthAlign:Dir = CENTER
			var eastWestAlign:Dir = CENTER
			var kerning = 5
			var textColor = Color.BLACK
			var font = Fonts.default
		}
}
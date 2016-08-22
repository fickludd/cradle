package se.jt.text

import java.awt.Font
import java.awt.FontMetrics
import java.awt.Canvas
import scala.collection.mutable.HashMap

object Fonts {
	
	def resourceFont(resourcePath:String) = {
		val is = classOf[String].getResourceAsStream(resourcePath)
		val font = Font.createFont(Font.TRUETYPE_FONT, is)
		is.close()
		font
	}
	
	val customFonts = new HashMap[String, Font]
	
	customFonts("OpenSans-Regular") = resourceFont("/Open_Sans/OpenSans-Regular.ttf")
	lazy val default = customFonts("OpenSans-Regular").deriveFont(12f)
	
	val logicalFonts = Array("Dialog", "DialogInput", "Serif", "SansSerif", "Monospaced")
	
	def getFont(id:String) = 
		customFonts.get(id).getOrElse(
				if (logicalFonts.contains(id)) 
					new Font(id, Font.PLAIN, 12)
				else Font.getFont(id)
			)
	
	def getFontMetrics(font:Font) =
		(new Canvas()).getFontMetrics(font)
		
	def toCharDel(dx:Int, fm:FontMetrics, str:String):Int = {
		val ws = str.map(fm.charWidth(_))
		var x0 = 0
		var x1 = 0
		for (i <- 0 until ws.length) {
			x0 = x1
			x1 = x1 + ws(i)
			if (x1 > dx) {
				if (math.abs(dx - x0) < math.abs(dx - x1))
					return i
				else return i+1
			}
		}
		return ws.length
	}
}
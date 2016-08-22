package se.jt.frame

import javax.imageio.ImageIO
import java.awt.Graphics2D
import java.net.URL

object Icon {
	
	class ImgIcon(val url:URL) extends Icon {
		val img = ImageIO.read(url)
		
		def w = img.getWidth
		def h = img.getHeight
		
		def render(g:Graphics2D, x:Int, y:Int) = {
			g.drawImage(img, null, x, y)
		}
	}
	
	def resource(path:String) = new ImgIcon(classOf[Icon].getResource(path))
	def file(path:String) = new ImgIcon(new URL("file:"+path))
}

trait Icon {
	def w:Int
	def h:Int
	
	def render(g:Graphics2D, x:Int, y:Int):Unit
}

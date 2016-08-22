package se.jt.frame

import java.awt.Color
import scala.util.Random

object Colors {
	
	case class HSVColor(h:Double, s:Double, v:Double)
	
	implicit def color2hsv(c:Color) = {
		val rgb = c.getRGBColorComponents(null)
		val cMax = rgb.max
		val cMin = rgb.min
		val d = cMax - cMin
		val h = 
			if (d == 0) 0
			else if (cMax == rgb(0)) 		60 * (((rgb(1) - rgb(2))/d) % 6)
			else if (cMax == rgb(1))	60 * (((rgb(2) - rgb(0))/d) + 2)
			else 						60 * (((rgb(0) - rgb(1))/d) + 4)
		val s =
			if (d == 0) 0
			else		d / cMax
		val v = cMax
		HSVColor(h, s, v)
	}
	
	implicit def hsv2color(hsv:HSVColor) = {
		val c = hsv.v * hsv.s
		val x = c * (1-math.abs((hsv.h / 60) % 2 - 1))
		val m = hsv.v - c
		val (r,g,b) =
			if (hsv.h < 60)			(c+m, x+m, m)
			else if (hsv.h < 120)	(x+m, c+m, m)
			else if (hsv.h < 180)	(m, c+m, x+m)
			else if (hsv.h < 240)	(m, x+m, c+m)
			else if (hsv.h < 300)	(x+m, m, c+m)
			else 					(c+m, m, x+m)
		new Color((255*r).toInt, (255*g).toInt, (255*b).toInt)
	}
	
	def darker(hsv:HSVColor) = HSVColor(hsv.h, hsv.s, hsv.v * 0.7)
	def lighter(hsv:HSVColor) = HSVColor(hsv.h, hsv.s, (1.0 - (1.0 - hsv.v) * 0.7))

	def interpolate(c1:Color, c2:Color, k:Float) = {
		if (k <= 0.0) c1
		else if (k >= 1.0) c2
		else {
			val f1 = new Array[Float](3)
			val f2 = new Array[Float](3)
			c1.getColorComponents(f1)
			c2.getColorComponents(f2)
			new Color(
					f1(0)*(1-k) + f2(0)*k, 
					f1(1)*(1-k) + f2(1)*k,
					f1(2)*(1-k) + f2(2)*k)
		}
	}
	
	def dist(c1:Color, c2:Color):Double = {
		val maxDist = 1.7320508075688772
		val f1 = new Array[Float](3)
		val f2 = new Array[Float](3)
		c1.getColorComponents(f1)
		c2.getColorComponents(f2)
		(maxDist - math.sqrt(f1(0)*f2(0)+f1(1)*f2(1)+f1(2)*f2(2))) / maxDist	
	}
	
	def random = {
		val r = Random.nextFloat
		val g = Random.nextFloat
		new Color(r, g, 1.0f - (r+g)/2)
	}
}
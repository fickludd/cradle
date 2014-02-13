package se.jt.frame

import java.awt.Color

object Colors {

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
}
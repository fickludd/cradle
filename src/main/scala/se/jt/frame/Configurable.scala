package se.jt.frame

import scala.collection.mutable.HashMap

trait Configurable extends Piece {

	protected val configs = new HashMap[String, String => Unit]
	
	def configure(vals:Seq[(String, String)]) = {
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
	}
}
package se.jt

import akka.actor.Actor
import akka.actor.Props

import se.jt.frame.Style

import java.io.File

object StyleLoader {
	trait StyleMsg
	case class ReloadStyle() extends StyleMsg
	case class LoadFailed(msg:String) extends StyleMsg
}

class StyleLoader(styleFile:File) extends Actor {

	import StyleLoader._
	
	def receive = {
		case ReloadStyle() =>
			try {
				val s = reloadStyle
				sender ! CradleMaster.ApplyStyle(s)
			} catch {
				case e:Exception =>
					sender ! LoadFailed(e.getMessage)
			}
	}
	
	def reloadStyle = 
		Style.fromFile(styleFile)

}
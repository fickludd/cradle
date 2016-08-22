package se.jt.test

import akka.actor.ActorSystem
import akka.actor.Props

import se.jt.frame.Style
import se.jt.CradleMaster
import se.jt.StyleLoader
import java.io.File

object Editor {
	
	def main(args:Array[String]): Unit = {
		val system = ActorSystem("EditorSystem")
		
		val styleFile = new File("src/test/resources/editor.style")
		val sl = system.actorOf(Props(new StyleLoader(styleFile)), name="styleLoader")
		val em = system.actorOf(Props(new EditorMaster(sl)), name="editMaster")	
	}
}
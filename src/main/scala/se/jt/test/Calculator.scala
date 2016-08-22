package se.jt.test

import akka.actor.ActorSystem
import akka.actor.Props

import se.jt.StyleLoader
import java.io.File

object Calculator {

	def main(args: Array[String]): Unit = {
		
		val system = ActorSystem("HelloSystem")

		val styleFile = new File("src/test/resources/calculator.style")
		val sl = system.actorOf(Props(new StyleLoader(styleFile)), name="styleLoader")
		val facComputer = system.actorOf(Props[FacComputer], name="facComputer")
		val cm = system.actorOf(Props(new CalcMaster(facComputer, sl)), name="calcmaster")
	}
	
}
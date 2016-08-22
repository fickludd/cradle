package se.jt

import akka.actor.Actor
import akka.actor.Props
import scala.concurrent.duration._

import se.jt.frame.Style


object CradleMaster {

	case class Frame()
	case class Initialize()
	case class ApplyStyle(style:Style)
}

trait CradleMaster extends Actor {

	import CradleMaster._
	import this.context.dispatcher
	import Style._
	
	self ! Initialize()
	//this.context.system.scheduler.schedule(30 milliseconds, 30 milliseconds, self, Frame())
	
	def cradle:Cradle 
	
	def receive = {
		case Initialize() =>
			cradle.initialize()
			self ! Frame()
		
		case Frame() =>
			for (input <- cradle.userInput.getInput) {
				try {
					cradle.handleUserInput(input)
				} catch {
					case e:Exception =>
						println(e.getMessage)
						e.printStackTrace()
					case t:Throwable =>
						t.printStackTrace()
				}
			}
			cradle.userInput.atEndOfLoop

			cradle.draw()
			
			this.context.system.scheduler.scheduleOnce(30 milliseconds, self, Frame())
		
		case ApplyStyle(style) =>
			for (x <- style(cradle)) 
				x match {
					case StyleFail(msg) => println("STYLEFAIL: "+msg)
					case StyleApplied() => {}
				}

		case a => handleMessage(a)
	}
	
	def handleMessage(a:Any):Unit = {}
}
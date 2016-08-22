package se.jt.test

import akka.actor._
import scala.concurrent.duration._
import se.jt.ProgressBinPiece

object FacComputer {
	
	case class Compute(x:Double)
	case class Progress()
}

class FacComputer extends Actor {
	
	import FacComputer._
	import ProgressBinPiece._
	import this.context.dispatcher
	
	var _x:Option[Double] = None
	var progress = 0.0
	var client:ActorRef = _
	
	def receive = {
		case Compute(x) =>
			_x = Some(x)
			client = sender
			reportProgress(0.0)
			
		case Progress() =>
			val p = progress + 0.02
			if (p >= 1.0) {
				client ! HasResult(1.until(_x.get.toInt+1).toIterable.fold(1)(_ * _))
			} else 
				reportProgress(p)
	}
	
	def reportProgress(p:Double) = {
		progress = p
		client ! Loading(p)
		this.context.system.scheduler.scheduleOnce(50 milliseconds, self, Progress())
	}

}
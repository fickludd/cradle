package se.jt.test

import se.jt.Cradle
import se.jt.CradleMaster
import se.jt.StyleLoader
import se.jt.FlexGridPiece
import se.jt.ButtonPiece
import se.jt.InputPiece
import se.jt.ProgressBinPiece
import se.jt.frame.Geom.Rect

import akka.actor.ActorRef
import scala.util.Try

import se.jt.event.Reactor
import se.jt.event.ReloadStyle

class CalcMaster(val facComputer:ActorRef, styleLoader:ActorRef) extends CradleMaster with Reactor {
	
	import se.jt.input.UserInput._

	val CALC_CHARS = Array('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'e', '.', ',', '+', '-', '*', '/', '!')
	val display = InputPiece("display")
	//display.configure(List("bgColor" -> "0xFFFFFF", "halign" -> "east"))
	display.charFilter = c => CALC_CHARS.contains(c)
	display.captures += {
		case PRINTABLECHAR(c, mods) =>
			if (Array('+', '-', '*', '/', '!').contains(c)) 
				evaluate(display.text.init) match {
					case Some(x) =>
						println(display.text + " = "+x)
						display.text_=(x + "" + c)
						
					case None =>
						println("Couldn't evaluate '"+display.text+"'")
				}
			Ignore
			
		case ENTER() =>
			calculate()
			Eat
	}
	
	val facProgress = ProgressBinPiece("facProgress", "get factorial", 
		_ match {
			case d:Double => display.text_=(""+d)
			case i:Int => display.text_=(""+i)
			case x => println("unknown result in facProgress: "+x)
		})
	
	lazy val cradle = new Cradle(
			"a calculator",
			FlexGridPiece("grid",
				(display, Rect(0, 0, 4, 1)),
				(facProgress, Rect(4, 0, 1, 1)),
				(numBut("0"), Rect(1, 4, 1, 1)),
				(numBut("1"), Rect(0, 3, 1, 1)),
				(numBut("2"), Rect(1, 3, 1, 1)),
				(numBut("3"), Rect(2, 3, 1, 1)),
				(numBut("4"), Rect(0, 2, 1, 1)),
				(numBut("5"), Rect(1, 2, 1, 1)),
				(numBut("6"), Rect(2, 2, 1, 1)),
				(numBut("7"), Rect(0, 1, 1, 1)),
				(numBut("8"), Rect(1, 1, 1, 1)),
				(numBut("9"), Rect(2, 1, 1, 1)),
				
				(colBut(".", comma _, "0xCCCCCC"), Rect(2, 4, 1, 1)),
				(colBut("e", exp _, "0xCCCCCC"), Rect(0, 4, 1, 1)),
				
				(colBut("+", add _, "0xCCEE77"), Rect(3, 1, 1, 1)),
				(colBut("-", sub _, "0xCCEE77"), Rect(3, 2, 1, 1)),
				(colBut("*", mul _, "0xCCEE77"), Rect(3, 3, 1, 1)),
				(colBut("/", div _, "0xCCEE77"), Rect(3, 4, 1, 1)),
				(colBut("!", fac _, "0xCCEE77"), Rect(4, 1, 1, 1)),
				
				(colBut("000", clear _, "0xEEBB99"), Rect(4, 2, 1, 1)),
				(colBut("enter", calculate _, "0x7799EE"), Rect(4, 3, 1, 2))
			))
	
	listenTo(cradle)
	reactions += {
		case ReloadStyle() =>
			styleLoader ! StyleLoader.ReloadStyle()
	}
	
	styleLoader ! StyleLoader.ReloadStyle()
	
	override def handleMessage(a:Any):Unit = a match {
		case s:ProgressBinPiece.State =>
			facProgress.setState(s)
		case _ => {}
	}
	
	
	def numBut(str:String) = 
		butConf(ButtonPiece(str, num(str.toInt) _), "0xFFFFFF") 
	
	def colBut(str:String, f:() => Unit, bgColor:String) = 
		butConf(ButtonPiece(str, f), bgColor)
	
	def butConf(b:ButtonPiece, bgColor:String) = {
		b.configure(List("bgColor" -> bgColor, "valign" -> "center", "halign" -> "center", "margin" -> "5px"))
		b
	}
	

	def num(n: Int)(): Unit = 
		display.text_=(display.text + n)

	def comma(): Unit = 
		display.text_=(display.text + '.')

	def exp(): Unit = 
		display.text_=(display.text + 'e')

		
	def add(): Unit = {
		calculate
		display.text_=(display.text + " + ")
	}
	
	def sub(): Unit = {
		calculate
		display.text_=(display.text + " - ")
	}

	def mul(): Unit = {
		calculate
		display.text_=(display.text + " * ")
	}

	def div(): Unit = {
		calculate
		display.text_=(display.text + " / ")
	}

	def fac(): Unit = {
		evaluate(display.text) match {
			case Some(d) =>
				facComputer ! FacComputer.Compute(d)
			
			case None =>
				println("can't compute factorial of '"+display.text+"'")
		}
	} 
		//display.text_=(display.text + "!")
	
		
	def clear():Unit = 
		display.text_=("")
		
	def calculate():Unit = 
		evaluate(display.text) match {
			case Some(x) =>
				println(display.text + " = "+x)
				display.text_=(""+x)
				
			case None =>
				println("Couldn't evaluate '"+display.text+"'")
		}
	
	def evaluate(expr:String):Option[Double] = {
		if (Try(expr.toDouble).isSuccess)
			Some(expr.toDouble)
		else if (expr.contains("+")) {
			val parts = expr.split("\\+", 2)
			println(parts)
			Some(parts(0).toDouble + parts(1).toDouble)
		} else if (expr.contains("-")) {
			val parts = expr.split("-", 2)
			Some(parts(0).toDouble - parts(1).toDouble)
		} else if (expr.contains("*")) {
			val parts = expr.split("\\*", 2)
			Some(parts(0).toDouble * parts(1).toDouble)
		} else if (expr.contains("/")) {
			val parts = expr.split("/", 2)
			Some(parts(0).toDouble / parts(1).toDouble)
		} else if (expr.contains("!")) {
			val num = expr.takeWhile(_ != '!').toDouble
			Some(1.until(num.toInt+1).toIterable.fold(1)(_ * _))
		} else
			None
	}
}
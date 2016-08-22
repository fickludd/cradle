package se.jt.frame

import se.jt.Cradle

import java.io.BufferedReader
import java.io.FileReader
import java.io.File

import scala.util.{Success, Failure, Try}
import scala.collection.mutable.ArrayBuffer

object Style {
	
	trait StyleApplicationResult
	case class StyleApplied() extends StyleApplicationResult
	case class StyleFail(msg:String) extends StyleApplicationResult
	
	import Cradle._
			
	def fromFile(f:File) = {
		val r = new BufferedReader(new FileReader(f))
		
		val whiteSpace = Array(' ', '\t', '\n', '\r')
		var line = r.readLine
		var pieceFunc:Cradle => Configurable = c => { throw new Exception("No piece specified") }
		val a = new ArrayBuffer[(Cradle => Configurable, String, String)]
		
		while (line != null) {
			if (line.trim != "") {
				if (line.startsWith("\t")) {
					val kv = line.split("=", 2)
					a += ((pieceFunc, kv(0).trim, kv(1).trim))
				} else if (line.startsWith("#")) {
					// IGNORE
				} else if (line.contains("=")){
					val kv = line.split("=", 2)
					val dots = kv(0).split("\\.")
					a += ((toPieceFunc(dots.init.mkString(".")), dots.last.trim, kv(1).trim))
				} else {
					pieceFunc = toPieceFunc(line)
				}
			}
			line = r.readLine
		}
		
		new Style(a)
	}
	
	def toPieceFunc(path:String):Cradle => Configurable = 
		(c:Cradle) => {
			
			val p = c.getPiece(InTree(path.trim.split("\\.").toList))
			p match {
				case Success(p) =>
					p match {
						case c:Configurable => c
						case _ => throw new Exception("piece is not a configurable") 
					}
				case Failure(m) =>
					throw new Exception("No piece at path '"+path+"'")
					
			}
			
		}
}




class Style(configs:Seq[(Cradle => Configurable, String, String)]) {
	
	import Style._
	
	def apply(c:Cradle):Seq[StyleApplicationResult] = {
		var needRepose = false
		val resp = 
			for (cfg <- configs) yield {
				val (f, key, value) = cfg
				try {
					f(c).configure(List(key -> value))
					if (key == "height" || key == "width") 
						needRepose = true
					StyleApplied()
				} catch {
					case e:Exception => StyleFail(e.getMessage) 
				}
			}
		if (needRepose)
			c.root.makeDirty
		resp
	}
}
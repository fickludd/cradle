package se.jt

import java.awt.Graphics2D
import java.awt.Color

import scala.util.Random

import se.jt.frame.Animation
import se.jt.frame.Piece
import se.jt.frame.PurePoserPiece
import se.jt.frame.Configurable
import se.jt.frame.Colors
import se.jt.frame.Icon
import se.jt.input.MouseResponsive
import se.jt.event.Publisher

object ProgressBinPiece {
	
	trait State
	case class Passive 				extends State
	case class InfLoading() 		extends State
	case class Loading(done:Double) extends State
	case class HasResult(a:Any) 	extends State
	case class Finished() 			extends State
	
	def apply(name:String) = new ProgressBinPiece(name, Passive())
	def apply(name:String, initState:State) = new ProgressBinPiece(name, initState)
	def apply(
		name:String,
		resultStr:String = "get result!",
		onResultClick:Any => Unit = a => {}
	) = new ProgressBinPiece(name, Passive(), resultStr, onResultClick)
	
	
	class ProgressAnimation(pbp:ProgressBinPiece) extends Animation {
		
		val DONE_MESS = "LOADING_COMPLETE"
		
		def animate(p:Piece, g:Graphics2D, underlyingColor:Color, t:Long) = {
			val (mRect,inRect) = p.renderFrame(g, underlyingColor)
			
			g.setPaint(Color.WHITE)
			g.fillRect(mRect.x, mRect.y, mRect.w, mRect.h)
			
			val fm = g.getFontMetrics()
			val th = fm.getHeight()
			val cx = mRect.w / 4
			val cy = mRect.h / 4
			
			for {
				iy <- 0 until 4 
				ix <- 0 until 4
			} {
				val (c,col) = pbp.state match {
					case Passive() => ('.', Color.LIGHT_GRAY)
					case InfLoading() => (Random.nextPrintableChar, Color.BLACK)
					case Loading(d) => 
						if (16*d > iy*4 + ix) (DONE_MESS(iy*4+ix), Color.BLACK)
						else (Random.nextPrintableChar, Color.LIGHT_GRAY)
					case _ => ('*', Color.BLACK)
				}
				val cw = fm.charWidth(c)
				g.setColor(col)
				g.drawString(c.toString, mRect.x + cx/2 + ix*cx - cw/2, mRect.y + cy/2 + iy*cy + fm.getAscent - th / 2)
			}
		}
	
		/**
		 * time in seconds when animation started
		 */
		def startTime():Long = 0L
		def hasEnded(t:Long):Boolean = false
	}
}

class ProgressBinPiece(
		val name:String, 
		initState:ProgressBinPiece.State,
		resultStr:String = "get result!",
		onResultClick:Any => Unit = a => {}
) extends PurePoserPiece with Publisher with MouseResponsive with Configurable.Text {

	import ProgressBinPiece._
	import se.jt.frame.Compass._
	
	val progAnim = Some(new ProgressAnimation(this))
	
	var state:State = _
	def setState(s:State) = {
		state = s
		animation = s match {
			case Passive() => progAnim
			case InfLoading() => progAnim
			case Loading(d) => progAnim
			case _ => None
		}
		makeDirty
	}
	setState(initState)
	
	
	var activeColor:Option[Color] = None
	var pressedColor:Option[Color] = None// = Color.DARK_GRAY
	var clickedColor:Option[Color] = None// = Color.BLUE
	
	val btn = ButtonPiece(resultStr, () => 
		state match {
			case HasResult(a) => onResultClick(a)
			case _ => {}
		})
	
	def pieces:Map[String,se.jt.frame.Piece] = 
		state match {
			case HasResult(a) => Map("done" -> btn)
			case _ => Map()
		}
	
	def framedRepose(x:Int, y:Int, w:Int, h:Int): Unit = {
		btn.pos = (x, y)
		btn.size = (w, h)
	}
}
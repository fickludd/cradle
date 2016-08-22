package se.jt.frame

import java.awt.Color
import java.awt.Graphics2D
import scala.collection.mutable.ArrayBuffer
import se.jt.event.Publisher
import se.jt.event.PreferredSizeChanged
import se.jt.input.UserInput

trait Piece extends Publisher {

	import Geom._

	override def toString: String = 
		"[x=%d y=%d w=%d h=%d]".format(x, y, w, h)

	protected var _padding 	= Frame.NO
	protected var _border 	= Frame.NO
	protected var _margin 	= Frame.NO
	
	private var _x: Int = 0
	private var _y: Int = 0
	def x = _x
	def y = _y
	
	private var _h: Int = -1
	private var _w: Int = -1
	def h = _h
	def w = _w
	
	def ph = (if (userPH > 0) userPH else defaultPH) + _border.h + _margin.h + _padding.h
	def pw = (if (userPW > 0) userPW else defaultPW) + _border.w + _margin.w + _padding.w
	private var userPH = -1
	private var userPW = -1
	def defaultPH:Int = 100
	def defaultPW:Int = 100
	
	def userPrefWidth(w:Int):Unit = {
		userPW = w
		publish(PreferredSizeChanged(this))
	}
	def userPrefHeight(h:Int):Unit = {
		userPH = h
		publish(PreferredSizeChanged(this))
	}
	
	protected var selected: Boolean = false
	protected var animation: Option[Animation] = None
	var isDirty: Boolean = true

	def makeDirty = isDirty = true

	def bgColor:Option[Color]
	def borderColor:Option[Color]
	
	def name: String

	def apply(path: TreePath.Path): Piece = path match {
		case Nil => this
		case k :: ks => throw new Exception
	}

	def realize(path: TreePath.Path): List[Piece] = path match {
		case Nil => List(this)
		case k :: ks => throw new Exception
	}

	def pos = (x, y)
	def pos_=(t: (Int, Int)) = {
		this._x = t._1
		this._y = t._2
		makeDirty
	}

	def size = (w, h)
	def size_=(t: (Int, Int)) = {
		_w = t._1
		_h = t._2
		makeDirty
	}
//	def freeSize = {
//		_w = -1
//		_h = -1
//		makeDirty
//	}
	
	def preferredSize = (pw, ph)
//	def preferredSize_=(t: (Int, Int)) = {
//		pw = t._1
//		ph = t._2
//		makeDirty
//	}

	//def layer = l
	//def layer_=(l:Int) = { this.l = l; makeDirty }

	def rect = Rect(x, y, w, h)
	def contentRect = rect.removeFrame(_padding).removeFrame(_border).removeFrame(_margin)
	def overlaps(r: Rect) =
		x + w >= r.x && x < r.x + r.w && y + h >= r.y && y < r.y + r.h
	def overlapsOrNil(r: Rect) =
		if (overlaps(r)) List(this) else Nil

	def wraps(x: Int, y: Int) = this.x <= x && x < (this.x + w) && this.y <= y && y < (this.y + h)
	def wrappedOrNil(x: Int, y: Int) =
		if (wraps(x, y)) List(this) else Nil

	def select() = {
		selected = true
		makeDirty
	}
	def deselect() = {
		selected = false
		makeDirty
	}

	/*
	def highlight() = {
		highlighted = true
		makeDirty
	}
	def dehighlight() = {
		highlighted = false
		makeDirty
	}
	*/

	def rerender(g: Graphics2D, x:Int, y:Int, w:Int, h:Int): Unit
	def renderFrame(g: Graphics2D, underlyingColor:Color) = {
		renderBorder(g, rect, _margin, underlyingColor)
		val mRect = rect.removeFrame(_margin)
		borderColor.foreach(c => renderBorder(g, mRect, _border, c))
		val inRect = mRect.removeFrame(_border)
		(mRect, inRect)
	}
	
	def framedRerender(g: Graphics2D, underlyingColor:Color) = {
		//val mRect = rect.removeFrame(_margin)
		//borderColor.foreach(c => renderBorder(g, mRect, _border, c))
		val (mRect, inRect) = renderFrame(g, underlyingColor) //mRect.removeFrame(_border)
		bgColor.foreach(c => {
			g.setColor(c)
			g.fillRect(inRect.x, inRect.y, inRect.w, inRect.h)
		})
		val contRect = inRect.removeFrame(_padding)
		
		rerender(g, contRect.x, contRect.y, contRect.w, contRect.h)
		
		if (selected) {
			g.setPaint(Color.WHITE)
			g.drawRect(mRect.x, mRect.y, mRect.w-1, mRect.h-1)
		} 
	}
	def render(g: Graphics2D, underlyingColor:Color, t: Long): Unit = {
		animation match {
			case Some(anim) =>
				anim.animate(this, g, underlyingColor, t - anim.startTime)
				if (anim.hasEnded(t - anim.startTime)) {
					animation = None
					makeDirty
				}
			case None =>
				if (isDirty) {
					isDirty = false
					framedRerender(g, underlyingColor)
				}
		}
	}
	
	def renderBorder(g:Graphics2D, size:Rect, thickness:Frame, col:Color) = {
		g.setPaint(col)
		g.fillRect(size.x, size.y, w, thickness.north)
		g.fillRect(size.x, size.y+size.h-thickness.south, w, thickness.south)
		g.fillRect(size.x, size.y, thickness.east, size.h)
		g.fillRect(size.x+size.w-thickness.west, size.y, thickness.west, size.h)
	}
	
	def cradle(c:se.jt.Cradle) = {}

	val captures = new Captures

	class Captures {
		import UserInput._
		
		type Capture = PartialFunction[UserInput.Base, InputResponse]
		
		protected val captures = new ArrayBuffer[Capture]
		def +=(pf: Capture) = captures += pf
		def -=(pf: Capture) = captures -= pf
		def isDefinedAt(ui: UserInput.Base) = captures.exists(_ isDefinedAt ui)
		def apply(ui: UserInput.Base):Set[InputResponse] = {
			(for {
				c <- captures
				if c isDefinedAt ui
			} yield c(ui)).toSet
		}
		
	}
}
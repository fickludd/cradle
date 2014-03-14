package se.jt.frame

import java.awt.Graphics2D
import scala.collection.mutable.ArrayBuffer

trait Piece {

	import Geom._

	override def toString: String = 
		"[x=%d y=%d w=%d h=%d]".format(x, y, w, h)

	protected var x: Int = 0
	protected var y: Int = 0
	private var _h: Int = -1
	private var _w: Int = -1
	def h = if (_h < 0) ph else _h
	def w = if (_w < 0) pw else _w
	protected var ph: Int = 100
	protected var pw: Int = 100
	//protected var highlighted:Boolean = false
	protected var selected: Boolean = false
	protected var animation: Option[Animation] = None
	var isDirty: Boolean = true
	//var mouseHandler:Option[MouseAct.Base => Option[Any]] = None

	def makeDirty = isDirty = true

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
		this.x = t._1
		this.y = t._2
		makeDirty
	}

	def size = (w, h)
	def size_=(t: (Int, Int)) = {
		_w = t._1
		_h = t._2
		makeDirty
	}
	def freeSize = {
		_w = -1
		_h = -1
		makeDirty
	}
	
	def preferredSize = (pw, ph)
	def preferredSize_=(t: (Int, Int)) = {
		pw = t._1
		ph = t._2
		makeDirty
	}

	//def layer = l
	//def layer_=(l:Int) = { this.l = l; makeDirty }

	def rect = Rect(x, y, w, h)
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

	def rerender(g: Graphics2D): Unit
	def render(g: Graphics2D, t: Long): Unit = {
		animation match {
			case Some(anim) =>
				anim.animate(this, g, t - anim.startTime)
				if (anim.hasEnded(t - anim.startTime)) {
					animation = None
					makeDirty
				}
			case None =>
				if (isDirty) {
					isDirty = false
					rerender(g)
				}
		}
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
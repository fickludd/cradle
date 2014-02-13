package se.jt.frame

import java.awt.Graphics2D
import scala.collection.mutable.HashSet

trait Piece {

	import Geom._

	override def toString: String = "[x=%d y=%d w=%d h=%d]".format(x, y, w, h)

	protected var x: Int = 0
	protected var y: Int = 0
	protected var h: Int = 0
	protected var w: Int = 0
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
		this.w = t._1
		this.h = t._2
		makeDirty
	}
	def preferredSize: Option[(Int, Int)] = None

	//def layer = l
	//def layer_=(l:Int) = { this.l = l; makeDirty }

	def rect = Rect(x, y, w, h)
	def overlaps(r: Rect) =
		x + w > r.x && x < r.x + r.w && y + h > r.y && y < r.y + r.h
	def overlapsOrNil(r: Rect) =
		if (overlaps(r)) List(this) else Nil

	def wraps(x: Int, y: Int) = this.x < x && x < (this.x + w) && this.y < y && y < (this.y + h)
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

	val captures = new Captures

	class Captures {
		type Capture = PartialFunction[UserInput.Base, Boolean]
		
		protected val captures = new HashSet[Capture]
		def +=(pf: Capture) = captures += pf
		def -=(pf: Capture) = captures -= pf
		def isDefinedAt(ui: UserInput.Base) = captures.exists(_ isDefinedAt ui)
		def apply(ui: UserInput.Base):Boolean =
			(for {
				c <- captures
				if c isDefinedAt ui
			} yield c(ui)).exists(x => x)
		
	}
}
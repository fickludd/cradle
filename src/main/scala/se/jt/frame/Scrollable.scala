package se.jt.frame

trait Scrollable extends Piece {
	
	protected var vx = 0
	protected var vy = 0
	protected var vw = -1
	protected var vh = -1
	
	def viewPortActive = vw >= 0 && vh >= 0
	def visibleSize = if (viewPortActive) (vw, vh) else (w, h)
	
	def viewPort = (vx, vy, vw, vh)
	def setViewPort(vx:Int, vy:Int, vw:Int, vh:Int):Unit = {
		setViewPortPos(vx, vy)
		setViewPortSize(vw, vh)
	}
	def setViewPortSize(vw:Int, vh:Int):Unit = {
		this.vw = vw
		this.vh = vh
		makeDirty
	}
	def setViewPortPos(vx:Int, vy:Int):Unit = {
		if (preferredSize.isDefined) {
			val ps = preferredSize.get
			this.vx = math.max(0, math.min(ps._1 - vw, vx))
			this.vy = math.max(0, math.min(ps._2 - vh, vy))
		} else {
			this.vx = vx
			this.vy = vy
		}
		makeDirty
	}
}
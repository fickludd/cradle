package se.jt.frame

object Geom {
	case class Rect(x:Int, y:Int, w:Int, h:Int) {
		def removeFrame(f:Frame):Rect = 
			Rect(x+f.west, y+f.north, w-f.w, h-f.h)
	}
	
	

	object Frame {
		def apply(k:Int) = new Frame(k,k,k,k)
		val NO = new Frame(0,0,0,0)
	}
	case class Frame(east:Int, west:Int, north:Int, south:Int) {	
		def w = west+east
		def h = north+south
		
		def withEast(newEast:Int) = Frame(newEast, west, north, south)
		def withWest(newWest:Int) = Frame(east, newWest, north, south)
		def withNorth(newNorth:Int) = Frame(east, west, newNorth, south)
		def withSouth(newSouth:Int) = Frame(east, west, north, newSouth)
	}
}
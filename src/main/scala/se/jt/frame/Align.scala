package se.jt.frame

object Align {
	trait NorthSouthAlign
	trait EastWestAlign
	
	case class Top() extends NorthSouthAlign
	case class Center() extends NorthSouthAlign
	case class Bottom() extends NorthSouthAlign
}
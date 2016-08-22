package se.jt.frame

object Compass {
	
	class Dir(val name:String) {
		override def toString = name
	}
	val NORTH = new Dir("north")
	val SOUTH = new Dir("south")
	val EAST = new Dir("east")
	val WEST = new Dir("west")
	
	val NORTH_EAST = new Dir("north-east")
	val NORTH_WEST = new Dir("north-west")
	val SOUTH_EAST = new Dir("south-east")
	val SOUTH_WEST = new Dir("south-west")
	
	val CENTER = new Dir("center")
	
	def dir(str:String) =
		str match {
			case "north" => NORTH
			case "south" => SOUTH
			case "east" => EAST
			case "west" => WEST
			
			case "north-east" => NORTH_EAST
			case "north-west" => NORTH_WEST
			case "south-east" => SOUTH_EAST
			case "south-west" => SOUTH_WEST
			
			case "center" => CENTER
		}
}
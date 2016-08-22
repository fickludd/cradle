package se.jt.text

case class TextPos(row:Int, col:Int) extends Ordered[TextPos] {
	
	import scala.math.Ordered.orderingToOrdered
  
	def compare(that: TextPos): Int = 
		(this.row, this.col) compare (that.row, that.col)
}
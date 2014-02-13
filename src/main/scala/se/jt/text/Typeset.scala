package se.jt.text

import scala.collection.mutable.ArrayBuffer

/**
* @preserve Knuth and Plass line breaking algorithm
*
*/

object Typeset {

	val INFINITY = 1000
	
	class KPObject {
		val width:Int	= 0
		def stretch:Int	= 0
		def shrink:Int	= 0
		def penalty:Int	= 0
		def flagged:Int	= 0
		def isForcedBreak = penalty == -INFINITY
	}
	/*
	 * Class representing a glyph or character.  Boxes have a fixed
     * width that doesn't change.
	 */
	case class Box(
			override val width:Int, 
			val text:String
	) extends KPObject {}
	
	/*
	 * Class representing a bit of glue.  Glue has a preferred width,
    but it can stretch up to an additional distance, and can shrink
    by a certain amount.  Line breaks can be placed at any point where
    glue immediately follows a box.
    */
	case class Glue(
			override val width:Int,
			override val stretch:Int,
			override val shrink:Int
	) extends KPObject {
		/*
		 * Return how long this glue should be, for the given adjustment
        ratio r.
		*/
		def computeWidth(r:Double) = 
			if (r > 0) 	width + r*shrink
			else		width + r*stretch
	}
	
	/* Class representing a penalty.  Negative penalty values
    encourage line breaks at a given point, and positive values
    discourage breaks.  A value of INFINITY either absolutely requires
    or forbids a break.  Penalties have a width of zero unless a break
    is taken at the penalty point, at which point the value of the
    penalty's 'width' attribute is used.
	 */
	case class Penalty(
			override val width:Int,
			override val penalty:Int,
			override val flagged:Int = 0
	) extends KPObject {}
	
	
	class BreakNode(
			var position:Int,
			var line:Int,
			var fitnessClass:Int,
			var totalWidth:Int,
			var totalStretch:Int,
			var totalShrink:Int,
			var demerits:Double,
			var previous:BreakNode = null
	) {
		override def toString:String = "BP(pos="+position+", line="+line+", demerits="+demerits+")"
	}
	
	// Simple test code.
	def main(args:Array[String]):Unit = {
	    val text = """Writing this summary was difficult, because there were no large themes
in the last two weeks of discussion.  Instead there were lots and lots
of small items; as the release date for 2.0b1 nears, people are
concentrating on resolving outstanding patches, fixing bugs, and
making last-minute tweaks.
Computes a Adler-32 checksum of string.  (An Adler-32
checksum is almost as reliable as a CRC32 but can be computed much
more quickly.)  If value is present, it is used as the
starting value of the checksum; otherwise, a fixed default value is
used.  This allows computing a running checksum over the
concatenation of several input strings.  The algorithm is not
cryptographically strong, and should not be used for
authentication or digital signatures."""
	    
	    val lineWidth = 100			// Line width to use for formatting
	    val fullJustify = false		// Boolean; if true, do full justification
	    def getWordSize(word:String) = word.length
	    
	    // Turn chunk of text into an ObjectList.
	    val L = new NodeList()
	    L.addParagraph(text, getWordSize)
	
	    // Compute the breakpoints
	    val lineLengths = List(120, 110, 100, 90, 80, 70, 60, 50, 40, 30)
	    //L.debug = true
	    val breaks = L.computeBreakpoints( lineLengths,
	                                    tolerance = 2)
	    println("L.length: "+L.length)
	    println(breaks.mkString)
	
	    //assert breaks[0] == 0
	    var lineStart = 0
	    var line = 0
	    for (breakpoint <- breaks.tail) {
	        val r = L.computeAdjustmentRatio(lineStart, breakpoint, line, lineLengths)
	        line = line + 1
	        for (i <- lineStart until breakpoint) {
	            val box = L(i)
	            box match {
	            	case Glue(wi, st, sh) =>
	            		val width = 
	            			if (fullJustify) 	Glue(wi, st, sh).computeWidth(r)
	            			else				1
	            		print("".padTo(width.toInt, ' '))
	            	case Box(wi, ch) =>
	            		print(ch)
	            	case _ => {}
	            }
	        }
	        lineStart = breakpoint + 1
	        println
	    }
	    println
	}
	
	
	
	
	
	class NodeList extends ArrayBuffer[KPObject] {
		
		var debug = false
		
		def addClosingPenalty = {
			this += new Penalty(0, INFINITY, 0)
			this += new Glue(0, INFINITY, 0)
			this += new Penalty(0, -INFINITY, 1)
		}
		
		def isFeasibleBreakpoint(i:Int) = {
			this(i) match {
				case Penalty(wi, pe, fl) =>
					pe < INFINITY
				case Glue(wi, st, sh) =>
					if (i>0)
						this(i-1) match {
							case Box(wi, ch) 	=> true
							case _				=> false
						}
					else false
				case _ => false
			}
		}
		
		def isForcedBreak(i:Int) = {
			this(i) match {
				case Penalty(wi, pe, fl) =>
					pe == -INFINITY
				case _ => false
			}
		}
		
		def clean(str:String, chars:Array[Char]):List[String] = {
			if (str == "") return Nil
			val (word, rest) = str.span(c => !chars.contains(c))
			word :: clean(rest.dropWhile(c => chars.contains(c)), chars)
		}
		
		val CLEAN_CHARS = Array(' ', '\t', '\n', '\r')
		def addParagraph(str:String, wordSize:String => Int) = {
			val words = clean(str, CLEAN_CHARS)
			val spaceSize = wordSize(" ") + 1
			
			def toBox(w:String) = Box(wordSize(w), w)
			for (word <- words.init) {
		        if (word == "@")	// Append forced break
		        	append( Penalty(0, -INFINITY) )
		        else {
		        	append( toBox(word) ) 
		        	append( Glue(spaceSize, 1, 1) ) 
		        }    
		    }
		    append( toBox(words.last) )
		    // Append closing penalty and glue
		    addClosingPenalty
		}
		
		val sumWidth = new ArrayBuffer[Int]
		val sumStretch = new ArrayBuffer[Int]
		val sumShrink = new ArrayBuffer[Int]
		
		def measureWidth(i:Int, j:Int):Int 		= sumWidth(j) - sumWidth(i)
		def measureStretch(i:Int, j:Int):Int 	= sumStretch(j) - sumStretch(i)
		def measureShrink(i:Int, j:Int):Int 	= sumShrink(j) - sumShrink(i)
		
		/*Compute adjustment ratio for the line between pos1 and pos2*/
	    def computeAdjustmentRatio(
	    		pos1:Int, 
	    		pos2:Int, 
	    		line:Int, 
	    		lineLengths:Seq[Int]
	    ):Double = {
	        val length =  
		        measureWidth(pos1, pos2) + (this(pos2) match {
		        	case Penalty(wi, pe, fl) => wi
		        	case _ => 0
		        }).toInt
	        
	        if (debug)
	            println("\tline length="+ length)
	
	        // Get the length of the current line; if the line_lengths list
	        // is too short, the last value is always used for subsequent
	        // lines.
	       
	        val availableLength = 
	        	if (line < lineLengths.length)	lineLengths(line)
		        else 							lineLengths.last
	
	        // Compute how much the contents of the line would have to be
	        // stretched or shrunk to fit into the available space.
	        val r = 
	        	if (length < availableLength) {
		            val y = measureStretch(pos1, pos2)
		            if (debug)
		                println("\tLine too short: shortfall = "+(availableLength - length)+
		                		", stretch = " + y)
		            if (y > 0)
		                (availableLength - length) / y.toDouble
			        else INFINITY
		        } else if (length > availableLength) {
		            val z = measureShrink(pos1, pos2)
		            if (debug)
		                println("\tLine too long: extra = "+(availableLength - length)+
		                		", shrink = " + z)
		            if (z > 0)
		            	(availableLength - length) / z.toDouble
		            else INFINITY
		        } else 0	// Exactly the right length!
		
	        return r
		}
		
		
		/*Add a node to the active node list.
        The node is added so that the list of active nodes is always
        sorted by line number, and so that the set of (position, line,
        fitness_class) tuples has no repeated values.
        */
		def addActiveNode(activeNodes:ArrayBuffer[BreakNode], node:BreakNode):Unit = {
	        
			var index = 0
	
	        // Find the first index at which the active node's line number
	        // is equal to or greater than the line for 'node'.  This gives
	        // us the insertion point.  
	        while (index < activeNodes.length &&
	               activeNodes(index).line < node.line)
	            index = index + 1
	           
	        var insertIndex = index
	
	        // Check if there's a node with the same line number and
	        // position and fitness.  This lets us ensure that the list of
	        // active nodes always has unique (line, position, fitness)
	        // values.        
	        while (index < activeNodes.length &&
	               activeNodes(index).line == node.line) {
	            if (activeNodes(index).fitnessClass == node.fitnessClass &&
	                activeNodes(index).position == node.position)
	                return // A match, so just return without adding the node
	            
	            index = index + 1
	        }
	
	        activeNodes.insert(insertIndex, node)
		}
		
		
		/*
		 * Compute a list of optimal breakpoints for the paragraph
        represented by this ObjectList, returning them as a list of
        integers, each one the index of a breakpoint.

        line_lengths : a list of integers giving the lengths of each
                       line.  The last element of the list is reused
                       for subsequent lines.
        looseness : An integer value. If it's positive, the paragraph
                   will be set to take that many lines more than the
                   optimum value.   If it's negative, the paragraph is
                   set as tightly as possible.  Defaults to zero,
                   meaning the optimal length for the paragraph.
        tolerance : the maximum adjustment ratio allowed for a line.
                    Defaults to 1.
        fitness_demerit : additional value added to the demerit score
                          when two consecutive lines are in different
                          fitness classes.
        flagged_demerit : additional value added to the demerit score
                          when breaking at the second of two flagged
                          penalties.
		 */
		def computeBreakpoints(
				lineLengths:Seq[Int],
                looseness:Int = 0,  // q in the paper
                tolerance:Int = 1,  // rho in the paper
                fitnessDemerit:Int = 100, // gamma (XXX?) in the paper
                flaggedDemerit:Int = 100 // alpha in the paper
        ):Seq[Int] = {
	   
		    if (isEmpty) return Nil            // No text, so no breaks
	
	        // Precompute lists containing the numeric values for each box.
	        // The variable names follow those in Knuth's description.
	        var w = new Array[Int](length)
	        var y = new Array[Int](length)
	        var z = new Array[Int](length)
	        var p = new Array[Int](length)
	        var f = new Array[Int](length)
	        for (i <- 0 until length) {
	            val obj = this(i)
	            w(i) = obj.width
	            obj match {
	            	case Glue(wi, st, sh) =>
	            		y(i) = st
	            		z(i) = sh
	            	case Penalty(wi, pe, fl) =>
	            		p(i) = pe
	            		f(i) = fl
	            	case _ => {}
	            }
	        }
	        
		    // Precompute the running sums of width, stretch, and shrink
	        // (W,Y,Z in the original paper).  These make it easy to measure the
	        // width/stretch/shrink between two indexes; just compute
	        // sum_*[pos2] - sum_*[pos1].  Note that sum_*[i] is the total
	        // up to but not including the box at position i.
	       
	        sumWidth.clear
	        sumStretch.clear
	        sumShrink.clear
		    var widthSum = 0
		    var stretchSum = 0
		    var shrinkSum = 0
	        
		    for (i <- 0 until length) {
	            sumWidth += widthSum
	            sumStretch += stretchSum
	            sumShrink += shrinkSum
	
	            var box = this(i)
	            widthSum   = widthSum + box.width
	            stretchSum = stretchSum + box.stretch
	            shrinkSum  = shrinkSum + box.shrink
		    }
	        
	        // Initialize list of active nodes to a single break at the
	        // beginning of the text.
	        var A = new BreakNode(0, 0, 1, 0, 0, 0, 0)
	        var activeNodes = ArrayBuffer[BreakNode](A)
	
	        if (debug)
	            println("Looping over "+length+" box objects")
	
	        for (i <- 0 until length) {
	            val B = this(i)
	            // Determine if this box is a feasible breakpoint and
	            // perform the main loop if it is.
	            if (isFeasibleBreakpoint(i)) {
	                 if (debug) {
	                     println("Feasible breakpoint at "+i+":\tCurrent active node list:" +
	                    		 activeNodes.mkString(" "))
	
	                     // Print the list of active nodes, sorting them
	                     // so they can be visually checked for uniqueness.
	                     /*def cmp_f(n1, n2):
	                         return cmp( (n1.line, n1.position, n1.fitness_class),
	                                     (n2.line, n2.position, n2.fitness_class) )
	                     active_nodes.sort(cmp_f)
	                     for A in active_nodes: print A.position, A.line, A.fitness_class
	                     print ; print*/
	                 }
	                 
	                 // Loop over the list of active nodes, and compute the fitness
	                 // of the line formed by breaking at A and B.  The resulting
	                 val breaks = new ArrayBuffer[BreakNode] // List of feasible breaks
	                 activeNodes = activeNodes.filter(A => {
	                	 var keep = true
	                     val r = computeAdjustmentRatio(A.position, i, A.line, lineLengths)
	                     if (debug) 
	                         println("\tr=" +r+ "\tline=" + A.line)
	
	                     // XXX is 'or' really correct here?  This seems to
	                     // remove all active nodes on encountering a forced break!
	                     if (r < -1 || B.isForcedBreak) {
	                         // Deactivate node A
	                         if (activeNodes.length == 1) {
	                             if (debug)
	                                 println("Can't remove last node!")
	                                 // XXX how should this be handled?
	                                 // Raise an exception?
	                         } else {
	                             if (debug)
	                                 println("\tRemoving node" + A)
	                             keep = false
	                         }
	                     }
	                     if (-1 <= r && r <= tolerance) {
	                         // Compute demerits and fitness class
	                    	 val T = 1 + 100 * math.pow(math.abs(r), 3)
	                         var demerits = 
	                        	 if (p(i) >= 0)
		                             math.pow(T + p(i), 3)
		                         else if (isForcedBreak(i))
		                             T * T - p(i)*p(i)
		                         else
		                             T * T
	
	                         demerits += (flaggedDemerit * f(i) * f(A.position))
	
	                         // Figure out the fitness class of this line (tight, loose,
	                         // very tight or very loose).
	                         val fitnessClass = 
	                        	 if   		(r < -.5) 0
	                        	 else if 	(r <= .5) 1
	                        	 else if 	(r <= 1)  2
	                        	 else 				  3
	
	                         // If two consecutive lines are in very
	                         // different fitness classes, add to the
	                         // demerit score for this break.
	                         if (math.abs(fitnessClass - A.fitnessClass) > 1)
	                             demerits = demerits + fitnessDemerit
	
	                         if (debug)
	                             println("\tDemerits="+ demerits + "\tFitness class=" + fitnessClass)
	
	                         // Record a feasible break from A to B
	                         val brk = new BreakNode(i, A.line + 1,
	                        		 		fitnessClass,
	                        		 		sumWidth(i),
	                        		 		sumStretch(i),
	                        		 		sumShrink(i),
	                        		 		demerits,
	                        		 		A)
	                         breaks.append(brk)
	                         if (debug)
	                             println("\tRecording feasible break" + B +
	                            		 "\t\tDemerits=" + demerits +
	                            		 "\t\tFitness class=" + fitnessClass)
	                     }
	                     keep
	                 }) // end for A in activeNodes
	                 if (breaks.nonEmpty) {
	                     if (debug)
	                         println("List of breaks at " + i + ":" + breaks.mkString(" "))
	                     for (brk <- breaks)
	                         addActiveNode(activeNodes, brk)
	                 }
	            }// end if isFeasibleBreakpoint()
	        } // end for i in range(m)
	       
	        if (debug)
	            println("Main loop completed, Active nodes=" + activeNodes.mkString(" "))
	
	        // Find the active node with the lowest number of demerits.
	        val L = activeNodes.map(A => (A.demerits, A))
	        L.sortBy(_._1)
	        A = L.head._2
	           
	        if (looseness != 0) {
	            // The search for the appropriate active node is a bit more
	            // complicated; we look for a node with a paragraph length
	            // that's as close as possible to (A.line+looseness), and
	            // with the minimum number of demerits.
	           
	            var best = 0
	            var d = INFINITY.toDouble
	            var b:BreakNode = null
	            for (br <- activeNodes) {
	                val delta = br.line - A.line
	                // The two branches of this 'if' statement
	                // are for handling values of looseness that are
	                // either positive or negative.
	                if ((looseness <= delta && delta < best) ||
	                    (best < delta && delta < looseness) ) {
	                    best = delta
	                    d = br.demerits
	                    b = br
	                } else if (delta == best && br.demerits < d) {
	                    // This break is of the same length, but has fewer
	                    // demerits and hence is a more attractive one.
	                    d = br.demerits
	                    b = br
	                }
	            }
	            A = b
	        }
	        
	        // Use the chosen node A to determine the optimum breakpoints,
	        // and return the resulting list of breakpoints.
	        val breaks = new ArrayBuffer[Int]
	        while (A != null) {
	            breaks.append( A.position )
	            A = A.previous
	        }
	        return breaks.reverse
		}
	}
}
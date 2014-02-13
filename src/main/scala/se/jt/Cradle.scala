package se.jt

import java.awt.Frame
import java.awt.Graphics2D
import java.awt.image.BufferedImage

import se.jt.frame.Piece
import se.jt.frame.PoserPiece
import se.jt.frame.UserInput
import se.jt.frame.TreePath

import scala.collection.mutable.DoubleLinkedList


object Cradle {
	
	def main(args:Array[String]):Unit = {
		
		import se.jt.frame.Compass._
		
		val list = SimpleListPiece[String]("testList", s => s)
		list.items_=(List("hi", "Hi", "ho", "HHOO"))
		list.size = (100, 100)
		
		val tableData = Array(
				Array(2.0, "johan.teleman@gmail.com", true),
				Array(2.0, "johan.teleman@gmail.com", true),
				Array(2.0, "johan.teleman@gmail.com", true),
				Array(2.0, "johan.teleman@gmail.com", true),
				Array(2.0, "johan.teleman@gmail.com", true),
				Array(2.0, "johan.teleman@gmail.com", true),
				Array(2.0, "johan.teleman@gmail.com", true),
				Array(2.0, "johan.teleman@gmail.com", true),
				Array(2.0, "johan.teleman@gmail.com", true),
				Array(2.0, "johan.teleman@gmail.com", true),
				Array(2.0, "johan.teleman@gmail.com", true),
				Array(2.0, "johan.teleman@gmail.com", true)
			)
		
		val c = new Cradle(
				"the title",
				BorderPiece("root",
						NORTH -> LabelPiece("North"),
						NORTH_WEST -> LabelPiece("NorthWEST"),
						SOUTH_EAST -> LabelPiece("SouthEAST"),
						SOUTH -> EastWestStack("EW-stack",
								LabelPiece("label1"), 
								InputPiece("input"), 
								LabelPiece("label3")),
						WEST -> ButtonPiece("West"),
						EAST -> list,
						CENTER -> NorthSouthSplit("ns-split",
								new TablePiece("table", tableData, List("value", "email", "is duplicate")),
								new ScrollPiece("scrollp", TextPiece("text", """Dear all, 
Monday January 27th at 15.00 in Bioforum, Sara Vikström is presenting her advanced course work entitled 

”Optimizing the scFv concentration coupled to magnetic beads in the AFFIRM platform”

Supervisors: Sofia Waldemarson, Anna Säll, Helena Persson
Examiner: Mats Ohlin

All very welcome!
/Sofia"""))
							)
				))
		
		run(c)
	}
	
	
	val fps = 60
	
	def run(c:Cradle) = {
		c.initialize()
		
        println("run() thread: "+Thread.currentThread)
        
        val start = System.currentTimeMillis()
        val timePerFrame = 1000 / fps
        
        while(true)
        {
            val before = 
           
            
            for (input <- c.userInput.getInput) {
            	c.handleUserInput(input)
            }
            c.userInput.atEndOfLoop
            
            c.draw()
           
            //  delay for each frame  -   time it took for one frame
            val t = System.currentTimeMillis()
            val toNextFrame = (timePerFrame - t % timePerFrame)
           
            try {
                Thread.sleep(toNextFrame)
            } catch { 
            	case e:Exception => {}
            }
            
        }
    }
}


class Cradle(
		val name:String,
		val root:PoserPiece
) extends Frame {
	
	val initWidth = 800
	val initHeight = 600
	
    var backBuffer:BufferedImage = null
    
    trait PiecePath
    case class InTree(path:TreePath.Path) extends PiecePath
    case class OverTree(node:DoubleLinkedList[Piece]) extends PiecePath
    
    var focus:Option[PiecePath] = None
    
    var highlight:Option[PiecePath] = None
    
    var userInput:UserInput = null
    
    def initialize() = {
    	setTitle(name)
        setSize(initWidth, initHeight)
        setVisible(true)
       
        val insets = getInsets()
        setSize(insets.left + initWidth + insets.right,
                        insets.top + initHeight + insets.bottom)
       
        root.size = (initWidth, initHeight)
        setResizable(false)
        
        userInput = new UserInput(this)
        
        import java.awt.event._
        addWindowListener(new WindowAdapter(){
        	override def windowClosing(we:WindowEvent) = {
        		System.exit(0)
        	}
		})
    	
        backBuffer = new BufferedImage(initWidth, initHeight, BufferedImage.TYPE_INT_RGB)
    }
	
	
	def setFocus(newPath:TreePath.Path) = {
		def fixNew = {
			root(newPath).select
			focus = Some(InTree(newPath))
			true
		}
		
		focus match {
			case Some(InTree(path)) =>
				if (path != newPath) {
					root(path).deselect
					fixNew
				} else false
			case _ =>
				fixNew
		}
	}
	
	
	def moveHighlight(newPath:TreePath.Path) = 
		highlight match {
			case Some(InTree(path)) =>
				if (path != newPath) {
					pushInput(path, UserInput.MouseLeave())
					pushInput(newPath, UserInput.MouseEnter())
					highlight = Some(InTree(newPath))
				}
			case _ =>
				pushInput(newPath, UserInput.MouseEnter())
				highlight = Some(InTree(newPath))
		}
	
	
	
	def pushInput(path:TreePath.Path, input:UserInput.Base):Unit = {
		val ps = root.realize(path)
		for (p <- ps)
			if (p.captures(input)) 
				return
	}
	
	
	def handleUserInput(input:UserInput.Base):Unit = {
		import UserInput._
		
		input match {
			case MouseClick(x, y) =>
				setFocus(root.getPathAt(x, y))
				println("new focus: "+ focus)
				
			case MouseMove(x, y, pressed) =>
				moveHighlight(root.getPathAt(x, y))
				
			case _ => {}
		}
		
		focus match {
			case Some(InTree(path)) => 
				pushInput(path, input)
			case _ => {}
		}
	}
	
	
    def draw() = {              
        val g = getGraphics()
       
        val bbg = backBuffer.getGraphics().asInstanceOf[Graphics2D]
        
        /*floatLayer = for ((p, rect) <- floatLayer) yield {
        	val nrect = p.rect
        	if (nrect != rect) {
        		for (under <- root.getTouchingPieces(nrect))
        			under.makeDirty
        	}
        	(p, nrect)
        }*/
        root.posePieces
        
        val t = System.currentTimeMillis()
        
        val pieces = root.renderTree(bbg, t)
        
        //for (f <- floatLayer) f._1.render(bbg)
        
        g.drawImage(backBuffer, insets.left, insets.top, this)
    }
}
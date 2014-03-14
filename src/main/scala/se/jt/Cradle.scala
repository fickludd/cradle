package se.jt

import java.awt.Frame
import java.awt.Graphics2D
import java.awt.image.BufferedImage

import se.jt.frame.Piece
import se.jt.frame.PoserPiece
import se.jt.frame.UserInput
import se.jt.frame.TreePath
import se.jt.frame.Geom.Rect

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
		
		val wrapView = WrapViewPiece("wrapView")
		wrapView.items_=(List(Filler(200, 150), Filler(180, 130), 
								Filler(110, 220)
							))
		
		val c = new Cradle(
				"the title",
				BorderPiece("root",
						NORTH -> HintputPiece("Hintput", str => List(str+"1", str+"2", str+"3")),
						NORTH_WEST -> LabelPiece("NorthWest"),
						SOUTH_EAST -> LabelPiece("SouthEAST"),
						SOUTH -> EastWestStack("EW-stack",
								LabelPiece("label1"), 
								InputPiece("input"), 
								new ScrollPiece("scrollp", TextPiece("text", """Dear all, 
Monday January 27th at 15.00 in Bioforum, Sara Vikstrom is presenting her advanced course work entitled 

ÓOptimizing the scFv concentration coupled to magnetic beads in the AFFIRM platformÓ

Supervisors: Sofia Waldemarson, Anna Sall, Helena Persson
Examiner: Mats Ohlin

All very welcome!
/Sofia"""))),
						WEST -> ButtonPiece("West"),
						EAST -> list,
						CENTER -> TabPiece("tabs!",
								wrapView,
								LabelPiece("another tag.. no tab!"),
								NorthSouthSplit("ns-split",
									new TablePiece("table", tableData, List("value", "email", "is duplicate")),
									new ScrollPiece("scrollp2", 
											GridPiece("grid", 
												List(LabelPiece("hi"), LabelPiece("yo")),
												List(LabelPiece("hi1"), LabelPiece("yo2")),
												List(LabelPiece("hi2"), LabelPiece("yo3")),
												List(LabelPiece("hi3"), LabelPiece("yo4")),
												List(LabelPiece("hi31"), LabelPiece("yo41")),
												List(LabelPiece("hi32"), LabelPiece("yo42")),
												List(LabelPiece("hi33"), LabelPiece("yo43")),
												List(LabelPiece("hi34"), LabelPiece("yo44")),
												List(LabelPiece("hi35"), LabelPiece("yo45"))
											))
								))
							)
				)
		
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
            //val before = 
           
            
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
	
    var floatLayer = new DoubleLinkedList[(Piece, Rect)]
	def registerFloat(p:Piece) = 
		floatLayer = floatLayer :+ p -> p.rect
	
	root.cradleTree(this)
	
    var backBuffer:BufferedImage = null
    
    trait PiecePath
    case class InTree(path:TreePath.Path) extends PiecePath
    case class OverTree(node:DoubleLinkedList[Piece]) extends PiecePath
    
    var highlightLock = false
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
					println("highlight: "+newPath)
					pushInput(newPath, UserInput.MouseEnter())
					highlight = Some(InTree(newPath))
				}
			case _ =>
				pushInput(newPath, UserInput.MouseEnter())
				highlight = Some(InTree(newPath))
		}
	
	
	
	def pushInput(path:TreePath.Path, input:UserInput.Base):Unit = {
		import UserInput._
		val ps = root.realize(path)
		for (p <- ps) {
			val resps = p.captures(input)
			if (resps contains Lock)
				highlightLock = true
			
			if (resps contains UnLock)
				highlightLock = false
			
			if (resps contains Eat)
				return
		}
	}
	
	
	def handleUserInput(input:UserInput.Base):Unit = {
		import UserInput._
		
		if (!highlightLock)
			input match {
				case MouseClick(x, y) =>
					setFocus(root.getPathAt(x, y))
					println("new focus: "+ focus)
					
				case MouseMove(x, y, pressed) =>
					moveHighlight(root.getPathAt(x, y))
					
				case _ => {}
			}
		
		input match {
			case mi:MouseInput =>
				highlight match {
					case Some(InTree(path)) =>
						pushInput(path, input)
					case _ => {}
				}
			case kbi:KeyBoardInput =>
				focus match {
					case Some(InTree(path)) => 
						pushInput(path, input)
					case _ => {}
				}
		}
	}
	
	
    def draw() = {              
        val g = getGraphics()
       
        val bbg = backBuffer.getGraphics().asInstanceOf[Graphics2D]
        
        floatLayer = 
        	for ((p, rect) <- floatLayer) yield {
	        	val nrect = p.rect
	        	if (nrect != rect) {
	        		for (under <- root.getTouchingPieces(rect))
	        			under.makeDirty
	        	}
	        	(p, nrect)
	        }
        root.posePieces()
        
        val t = System.currentTimeMillis()
        
        val pieces = root.renderTree(bbg, t)
        
        for (f <- floatLayer) f._1.render(bbg, t)
        
        g.drawImage(backBuffer, insets.left, insets.top, this)
    }
}
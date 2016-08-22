package se.jt

import java.awt.Frame
import java.awt.Graphics2D
import java.awt.Color
import java.awt.image.BufferedImage
import java.awt.Font
import java.io.InputStream

import se.jt.frame.Piece
import se.jt.frame.PoserPiece
import se.jt.input.UserInput
import se.jt.frame.TreePath
import se.jt.frame.Geom.Rect
import se.jt.frame.Icon

import se.jt.event.Publisher
import se.jt.event.Reactor
import se.jt.event.PreferredSizeChanged
import se.jt.event.ReloadStyle

import scala.collection.mutable.DoubleLinkedList
import scala.util.{Try, Failure, Success}

object Cradle {
	
	trait PiecePath
    case class InTree(path:TreePath.Path) extends PiecePath
    case class OverTree(node:DoubleLinkedList[Piece]) extends PiecePath
    
    
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
						SOUTH_EAST -> ProgressBinPiece("progress", ProgressBinPiece.Passive()),//ProgressBinPiece.Loading(0.7)),
						SOUTH -> EastWestStack("EW-stack",
								ButtonPiece("label1", Icon.file("src/test/resources/test_icon.png")), 
								InputPiece("input"), 
								ReadPiece("text", """Dear all, 
Monday January 27th at 15.00 in Bioforum, Sara Vikstrom is presenting her advanced course work entitled 

ÓOptimizing the scFv concentration coupled to magnetic beads in the AFFIRM platformÓ

Supervisors: Sofia Waldemarson, Anna Sall, Helena Persson
Examiner: Mats Ohlin

All very welcome!
/Sofia""")),
						WEST -> ToggleButtonPiece("West"),
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
	
	class FloatLayout extends Reactor {
		
		reactions += {
			case PreferredSizeChanged(float) => float.size = (float.pw, float.ph) 
		}
	}
}


class Cradle(
		val name:String,
		val root:PoserPiece
) extends Frame with Publisher {
	
	import Cradle._
	
	lazy val awtFrame = this //new Frame
	
	val initWidth = 800
	val initHeight = 600
	
    var floatLayer = new DoubleLinkedList[(Piece, Rect)]
	val floatLayout = new FloatLayout
	def registerFloat(p:Piece) = {
		p.size = (p.pw, p.ph)
		floatLayout.listenTo(p)
		floatLayer = floatLayer :+ p -> p.rect
	}
	
	root.cradleTree(this)
	
    var backBuffer:BufferedImage = null
    
    var highlightLock = false
    var focus:Option[PiecePath] = None
    var highlight:Option[PiecePath] = None
    
    var userInput:UserInput = null
    
    def initialize() = {
    	awtFrame.setTitle(name)
        awtFrame.setSize(initWidth, initHeight)
        awtFrame.setVisible(true)
       
        val insets = awtFrame.getInsets()
        awtFrame.setSize(insets.left + initWidth + insets.right,
                        insets.top + initHeight + insets.bottom)
                        
    	
        setupSize(initWidth, initHeight)
        
        userInput = new UserInput(awtFrame)
        
        import java.awt.event._
        awtFrame.addWindowListener(new WindowAdapter() {
        	override def windowClosing(we:WindowEvent) = {
        		System.exit(0)
        	}
		})
		awtFrame.addComponentListener(new ComponentAdapter() {
		    override def componentResized(e:ComponentEvent) {
		        val size = awtFrame.getSize
		    	val insets = awtFrame.getInsets
		        setupSize(
		        		size.getWidth.toInt - insets.left - insets.right, 
		        		size.getHeight.toInt - insets.top - insets.bottom)
		    }
		});
    }
	
	
	def setupSize(w:Int, h:Int) = {
		root.size = (w, h)
		backBuffer = new BufferedImage(w, h, BufferedImage.TYPE_INT_RGB)
	}
	
		
	def getPiece(path:PiecePath):Try[Piece] = {
		path match {
			case InTree(tp) => 
				tp match {
					case Nil => Failure(new Exception("Empty path"))
					case x::xs => 
						if (x != root.name) Failure(new Exception("Non matching roots"))
						else Try(root(xs))
				}
			case OverTree(node) => Try(node.head)
		}
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
		
		input match {
			case PRINTABLECHAR('Ã', mods) => publish(ReloadStyle())
			case _ => {}
		}
		
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
        val g = awtFrame.getGraphics()
       
        val bbg = backBuffer.getGraphics().asInstanceOf[Graphics2D]
        
        floatLayer = 
        	for ((p, rect) <- floatLayer) yield {
	        	val nrect = p.rect
	        	if (nrect != rect && rect.w > 0 && rect.h > 0) {
	        		for (under <- root.getTouchingPieces(rect))
	        			under.makeDirty
	        	}
	        	(p, nrect)
	        }
        root.posePieces()
        
        val t = System.currentTimeMillis()
        
        val pieces = root.renderTree(bbg, Color.BLACK, t)
        
        for (f <- floatLayer) f._1.render(bbg, Color.BLACK, t)
        
        g.drawImage(backBuffer, awtFrame.insets.left, awtFrame.insets.top, awtFrame)
    }
}
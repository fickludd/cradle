package se.jt.frame

import java.awt.Frame
import java.awt.event._

import scala.collection.mutable.SynchronizedQueue

object UserInput {
	trait Base
	case class Mods(shift:Boolean, alt:Boolean, ctrl:Boolean)
	
	trait MouseInput extends Base
	case class MousePress(x:Int, y:Int) extends MouseInput
	case class MouseRelease(x:Int, y:Int, obj:Option[Any]) extends MouseInput
	case class MouseMove(x:Int, y:Int, pressed:Boolean) extends MouseInput
	case class MouseClick(x:Int, y:Int) extends MouseInput
	case class MouseExitCradle(x:Int, y:Int) extends MouseInput
	case class MouseEnter() extends MouseInput
	case class MouseLeave() extends MouseInput
	
	trait KeyBoardInput extends Base
	case class ENTER extends KeyBoardInput
	case class BACKSPACE extends KeyBoardInput
	case class TAB extends KeyBoardInput
	case class PRINTABLECHAR(c:Char, mods:Mods) extends KeyBoardInput
	case class ARROW(dir:Compass.Dir, mods:Mods) extends KeyBoardInput
	
	class InputResponse
	val Ignore = new InputResponse
	val Eat = new InputResponse
	val Lock = new InputResponse
	val UnLock = new InputResponse
}


/**
 * Makes handling input a lot simpler
 */
class UserInput(f: Frame) extends MouseAdapter with KeyListener {
	
	
	
	val keys = new Array[Boolean](256)
	val insets = f.getInsets()
	
	var mx = 0
	var my = 0
	var mmoved = false
	var mclicked = false
	var mPressTime = 0
	var mPressPos = (0, 0)
	
	protected val input = new SynchronizedQueue[UserInput.Base]()
	
	def getInput() = 
		input.dequeueAll(_ => true)
	
	
	
	f.addKeyListener(this)
	f.addMouseMotionListener(this)
	f.addMouseListener(this)

	/**
	 * by user OscarRyz at stackoverflow.com
	 */
	def isPrintable(c:Char) = {
		val block = Character.UnicodeBlock.of( c );
		(!Character.isISOControl(c)) &&
            c != KeyEvent.CHAR_UNDEFINED &&
            block != null &&
            block != Character.UnicodeBlock.SPECIALS;
	}
	
	/**
	 * Checks whether a specific key is down
	 * @param keyCode The key to check
	 * @return Whether the key is pressed or not
	 */
	def isKeyDown(keyCode: Int): Boolean = {
		if (keyCode > 0 && keyCode < 256) {
			return keys(keyCode);
		}

		return false;
	}

	/**
	 * Called when a key is pressed while the component is focused
	 * @param e KeyEvent sent by the component
	 */
	def keyPressed(e: KeyEvent) = {
		if (e.getKeyCode() > 0 && e.getKeyCode() < 256) {
			keys(e.getKeyCode()) = true;
		}
		
		import UserInput._
		import Compass._
		
		val mods = Mods(e.isShiftDown(), e.isAltDown(), e.isControlDown())
		
		//println(KeyEvent.getKeyText(e.getKeyCode) + " - " + KeyEvent.getKeyModifiersText(e.getModifiers))
		if (e.getKeyCode() == KeyEvent.VK_ENTER)
			input += ENTER()
		else if (e.getKeyCode() == KeyEvent.VK_BACK_SPACE)
			input += BACKSPACE()
			
		else if (e.getKeyCode() == KeyEvent.VK_UP)
			input += ARROW(NORTH, mods)
		else if (e.getKeyCode() == KeyEvent.VK_DOWN)
			input += ARROW(SOUTH, mods)
		else if (e.getKeyCode() == KeyEvent.VK_LEFT)
			input += ARROW(WEST, mods)
		else if (e.getKeyCode() == KeyEvent.VK_RIGHT)
			input += ARROW(EAST, mods)
		
		else if (isPrintable(e.getKeyChar))
			input += PRINTABLECHAR(e.getKeyChar, mods)
		
	}

	def keyReleased(e: KeyEvent) = {
		if (e.getKeyCode() > 0 && e.getKeyCode() < 256) {
			keys(e.getKeyCode()) = false;
		}
	}

	override def mouseMoved(e: MouseEvent) = {
		mx = e.getX - insets.left
		my = e.getY - insets.top
		input += UserInput.MouseMove(mx, my, false)
		mmoved = true
	}

	override def mouseDragged(e: MouseEvent) = {
		mx = e.getX - insets.left
		my = e.getY - insets.top
		input += UserInput.MouseMove(mx, my, true)
		mmoved = true
	}
	
	override def mouseClicked(e:MouseEvent) = {
		//println("mouse handle thread: "+Thread.currentThread)
		input += UserInput.MouseClick(mx, my)
		mclicked = true
	}
	
	override def mousePressed(e:MouseEvent) = {
		//println("mouse handle thread: "+Thread.currentThread)
		mPressTime = 1
		mPressPos = (e.getX - insets.left, e.getY - insets.top)
		input += UserInput.MousePress(mPressPos._1, mPressPos._2)
	}
	
	override def mouseReleased(e:MouseEvent) = {
		mPressTime = -1
		mx = e.getX - insets.left
		my = e.getY - insets.top
		input += UserInput.MouseRelease(mx, my, None)
	}
	
	def mouseAlive =
		mmoved || mclicked || mPressTime != 0
	
	def atEndOfLoop = {
		//input = Nil
		mclicked = false
		mmoved = false
		if (mPressTime < 0) {
			mPressPos = (0, 0)
			mPressTime = 0
		} else if (mPressTime > 0) {
			mPressTime += 1
		}
	}
	

	/**
	 * Not used
	 */
	def keyTyped(e: KeyEvent) {}
} 
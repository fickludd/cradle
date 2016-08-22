package se.jt.frame

import java.awt.Graphics2D
import java.awt.Color

trait Animation {
	
	/**
	 * @p	piece to be animated
	 * @g	graphics where drawing should be done
	 * @t	time in seconds from start of animation
	 */
	def animate(p:Piece, g:Graphics2D, underlyingColor:Color, t:Long)
	
	/**
	 * time in seconds when animation started
	 */
	def startTime():Long
	def hasEnded(t:Long):Boolean
}
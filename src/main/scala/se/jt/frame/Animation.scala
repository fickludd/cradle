package se.jt.frame

import java.awt.Graphics2D

trait Animation {
	
	/**
	 * @p	piece to be animated
	 * @g	graphics where drawing should be done
	 * @t	time in seconds from start of animation
	 */
	def animate(p:Piece, g:Graphics2D, t:Long)
	
	/**
	 * time in seconds when animation started
	 */
	def startTime():Long
	def hasEnded(t:Long):Boolean
}
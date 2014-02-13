package se.jt.event

trait Event
case class TypingDone(p:se.jt.frame.Piece) extends Event
case class InputDone(p:se.jt.frame.Piece) extends Event
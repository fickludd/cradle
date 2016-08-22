package se.jt.event

trait Event
case class TypingDone(p:se.jt.frame.Piece) extends Event
case class InputDone(p:se.jt.frame.Piece) extends Event
case class Selection[T](p:se.jt.frame.Piece, t:T) extends Event

case class PreferredSizeChanged(p:se.jt.frame.Piece) extends Event
case class ReloadStyle() extends Event
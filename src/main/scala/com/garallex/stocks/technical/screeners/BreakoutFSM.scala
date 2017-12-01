package com.garallex.stocks.technical.screeners

import com.garallex.stocks.technical.screeners.BreakoutFSM._

object BreakoutFSM {

  sealed trait State

  case object InitialState extends State

  case class PreTouchPoint(nextNumber: Int) extends State

  case class TouchPoint(number: Int) extends State

  case object Match extends State

  case object Fail extends State

  sealed trait Message

  case object BodyCut extends Message // a

  case object ShadowCut extends Message // b

  case object Below extends Message // d

  case object Above extends Message // e

  def apply(state: State = InitialState, k: Int = 0): BreakoutFSM = new BreakoutFSM(state, k)
}

case class BreakoutFSM(state: State, k: Int = 0) {
  private final val TouchPointsToMatch = 3
  private final val NonContactPointsAllowedInsideSameContactPoint = 2

  def logAndReceive(message: Message): BreakoutFSM = {
    val newFsm = receive(message)
    println(s"$message: $this -> $newFsm")
    newFsm
  }

  def receive(message: Message): BreakoutFSM = (state, message) match {
    case (InitialState, BodyCut) => this
    case (InitialState, ShadowCut) => this

    case (InitialState, Below) => BreakoutFSM(PreTouchPoint(1))

    case (PreTouchPoint(_), Below) => this

    case (PreTouchPoint(nextNumber), BodyCut) => BreakoutFSM(TouchPoint(nextNumber))
    case (PreTouchPoint(nextNumber), ShadowCut) => BreakoutFSM(TouchPoint(nextNumber))

    case (TouchPoint(_), BodyCut) => this
    case (TouchPoint(_), ShadowCut) => this
    case (TouchPoint(number), Below) if number <= TouchPointsToMatch && k < NonContactPointsAllowedInsideSameContactPoint =>
      BreakoutFSM(TouchPoint(number), k + 1)

    case (TouchPoint(number), Below) if number < TouchPointsToMatch && k == NonContactPointsAllowedInsideSameContactPoint =>
      BreakoutFSM(PreTouchPoint(number + 1))

    case (TouchPoint(number), Below) if number == TouchPointsToMatch && k == NonContactPointsAllowedInsideSameContactPoint =>
      BreakoutFSM(Match)

    case (InitialState, Above) => BreakoutFSM(Fail)
    case (PreTouchPoint(_), Above) => BreakoutFSM(Fail)
    case (TouchPoint(_), Above) => BreakoutFSM(Fail)
    case (Fail, _) => BreakoutFSM(Fail)
    case (Match, _) => BreakoutFSM(Match)
  }
}

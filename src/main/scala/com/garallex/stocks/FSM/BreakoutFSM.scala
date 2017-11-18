package com.garallex.stocks.FSM

import com.garallex.stocks.FSM.BreakoutFSM._

object BreakoutFSM {

  sealed trait State

  case object InitialState extends State

  case class PreTouchPoint(touchPointNumber: Int) extends State

  case class TouchPoint(touchPointNumber: Int) extends State

  case object BreakoutMatch extends State

  case object BreakoutFail extends State

  sealed trait Message

  case object BodyCut extends Message // a

  case object ShadowCut extends Message // b

  case object BodyTopWithinDelta extends Message // c

  case object BodyTopLowerThanDelta extends Message // d

  case object BodyBottomHigherThanDelta extends Message // e

  def apply(state: State = InitialState, k: Int = 0): BreakoutFSM = new BreakoutFSM(state, k)
}

case class BreakoutFSM(state: State, k: Int = 0) {
  private final val N = 3
  private final val K = 2

  def logAndReceive(message: Message): BreakoutFSM = {
    val newFsm = receive(message)
    print(s"$message: $this -> $newFsm\n")
    newFsm
  }

  def receive(message: Message): BreakoutFSM = (state, message) match {
    case (InitialState, BodyCut) => BreakoutFSM(InitialState)
    case (InitialState, ShadowCut) => BreakoutFSM(InitialState)
    case (InitialState, BodyTopWithinDelta) => BreakoutFSM(InitialState)

    case (InitialState, BodyTopLowerThanDelta) => BreakoutFSM(PreTouchPoint(1))

    case (PreTouchPoint(i), BodyTopLowerThanDelta) => BreakoutFSM(PreTouchPoint(i))

    case (PreTouchPoint(i), BodyCut) => BreakoutFSM(TouchPoint(i))
    case (PreTouchPoint(i), ShadowCut) => BreakoutFSM(TouchPoint(i))
    case (PreTouchPoint(i), BodyTopWithinDelta) => BreakoutFSM(TouchPoint(i))

    case (TouchPoint(i), BodyCut) => BreakoutFSM(TouchPoint(i))
    case (TouchPoint(i), ShadowCut) => BreakoutFSM(TouchPoint(i))
    case (TouchPoint(i), BodyTopWithinDelta) => BreakoutFSM(TouchPoint(i))
    case (TouchPoint(i), BodyTopLowerThanDelta) if i <= N && k < K => BreakoutFSM(TouchPoint(i), k + 1)
    case (TouchPoint(i), BodyTopLowerThanDelta) if i < N && k == K => BreakoutFSM(PreTouchPoint(i + 1))
    case (TouchPoint(i), BodyTopLowerThanDelta) if i == N && k == K => BreakoutFSM(BreakoutMatch)

    case (InitialState, BodyBottomHigherThanDelta) => BreakoutFSM(BreakoutFail)
    case (PreTouchPoint(_), BodyBottomHigherThanDelta) => BreakoutFSM(BreakoutFail)
    case (TouchPoint(_), BodyBottomHigherThanDelta) => BreakoutFSM(BreakoutFail)
    case (BreakoutFail, _) => BreakoutFSM(BreakoutFail)
    case (BreakoutMatch, _) => BreakoutFSM(BreakoutMatch)
  }
}

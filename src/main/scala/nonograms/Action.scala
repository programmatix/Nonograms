package nonograms

// Actions a player can try to do on the board
trait Action

case class ActionMark(row: Int, col: Int) extends Action
case class ActionDelete(row: Int, col: Int) extends Action

trait ActionResult
case class ActionSuccess(newState: BoardState) extends ActionResult
case class ActionFailure() extends ActionResult
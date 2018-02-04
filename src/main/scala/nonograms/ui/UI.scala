package nonograms.ui

import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}



@JSExportTopLevel("UIMain")
object UIMain {

  @JSExport
  def main(containerId: String): Unit = {
//    new UISolver(containerId)
    new UIPlayer(containerId)
  }
}

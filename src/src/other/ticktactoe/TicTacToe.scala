package src.other.ticktactoe

import java.awt.EventQueue

object TicTacToe {

  def main(args: Array[String]): Unit = {
    EventQueue.invokeLater(() =>
      try {
        new Window
      } catch {
        case e: Exception =>
          e.printStackTrace()
      }
    )
  }

}

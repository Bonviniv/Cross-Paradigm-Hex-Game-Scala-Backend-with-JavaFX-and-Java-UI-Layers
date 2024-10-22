package Hex

import Hex.boardObj.Board
import Hex.projeto.{randomMove}
import Hex.projetoUtilis.{alterarBoardTUI}
import scala.annotation.tailrec

object projetoUtilisTUI {


  def endTui(player:Cells.Cell): Unit = {
    println(s"--------------- Player ${player.toString} wins ----------------------")
  }

  def getUserLoop(player: Cells.Cell, board: Board, size: Int): Coord = {
    println(s"Player ${player.toString} - Please enter a valid coordinate:")
    print("y:")
    try {
      val row = scala.io.StdIn.readInt()
      print("x:")
      val col = scala.io.StdIn.readInt()
      if (row >= 0 && row < size && col >= 0 && col < size && board(row)(col) == Cells.Empty) {
        Coord(row, col)
      } else {
        print("\nJogue outra casa:")
        getUserLoop(player, board, size)
      }
    } catch {
      case _: Throwable =>
        print("\nJogue outra casa:")
        getUserLoop(player, board, size)
    }
  }


  def undoTUI(pB: Board, board: Board): (Board, Boolean) = {
    println("Queres dar undo? (Escreve 'sim' se queres)")
    val input = scala.io.StdIn.readLine()
    if (input == "sim") {
      (pB, true)
    } else {
      (board, false)
    }
  }

  @tailrec
  def undoTUIloop(pb: Board, board: Board, size: Int, player: Cells.Cell, rand: MyRandom): Board = {
    val ranMovund = randomMove(board, rand)
    val res = undoTUI(pb, board)
    if (!res._2) {
      board
    } else {
      val nb = alterarBoardTUI(res._1, size, player, rand)
      undoTUIloop(pb, nb, size, player, (ranMovund._2))
    }


  }
}

package Hex

import Hex.boardObj.Board
import Hex.projeto.{printBoard, randomMove}
import Hex.projetoUtilisTUI.getUserLoop
import scala.annotation.tailrec

object projetoUtilis {


  def randomMoveCells(r: MyRandom): ((Int, Int), RandomWithState) = {
    val i = r.randomPar(r)
    (i._1, i._2)
  }

  def makeEmptyBoard(x: Int): Board = List.tabulate(x, x) {
    case (_, _) => Cells.Empty
  }


    ///////////////////////////////////////////////////////////

    def getUserInputgui(): Coord = {
      getUserInputgui()
    }

    ////////////////////////////////////////////////////////

    def valide(coord: Coord, board: Board, size: Int): Boolean = {
      val row = coord.row
      val col = coord.col
      if (row >= 0 && row < size && col >= 0 && col < size && board(row)(col) == Cells.Empty) true else false
    }

    def aply(coord: Coord, board: Board, player: Cells.Cell): Board = {
      val row = coord.row
      val col = coord.col
      val newBoard = board.map(_.toList)
      val newBoardRow = newBoard(row)
      val newBoardRowUpdated = newBoardRow.updated(col, player)
      val newBoardUpdated = newBoard.updated(row, newBoardRowUpdated)
      newBoardUpdated
    }

    @tailrec
    def alterarBoardGUI(board: Board, size: Int): Board = {
      val coord = getUserInputgui()
      if (!valide(coord, board, size)) alterarBoardGUI(board, size) else aply(coord, board, Cells.Red)
    }


    def alterarBoardTUI(board: Board, size: Int, player: Cells.Cell, rand: MyRandom): Board = {
      if(board==makeEmptyBoard(size))printBoard(board, size)
      val ranMov = randomMove(board, rand)
      player match {
        case Cells.Red =>
          val np=aply(getUserLoop(player, board, size), board, player)
          printBoard(np, size)
          np
        case _ =>
          val np=aply(ranMov._1, board, player)
          printBoard(np, size)
          np
      }
    }


  def connected(board: Board, cell1: (Int, Int), cell2: (Int, Int)): Boolean = {
    val x1 = cell1._1
    val x2 = cell2._1
    val y1 = cell1._2
    val y2 = cell2._2
    if (board(x1)(y1) == board(x2)(y2)) {
      if (cell1 == cell2)
        return false
      if (x1 == x2 || x1 == x2 + 1 || x1 == x2 - 1) {
        if (y1 == y2 || y1 == y2 + 1 || y1 == y2 - 1) {
          return true
        }
      }
    }
    false
  }


  def playerList(board: Board, player: Cells.Cell): List[(Int, Int)] = {
    val boardSize = board.length
    (0 until boardSize).foldLeft(List.empty[(Int, Int)]) { (acc, i) =>
      (0 until boardSize).foldLeft(acc) { (Accacc, j) =>
        if (board(i)(j) == player)
          (i, j) :: Accacc
        else
          Accacc
      }
    }
  }


  def getContinuousPaths(board: Board, player: Cells.Cell, coord: (Int, Int)): List[List[(Int, Int)]] = {
    val coords = playerList(board, player)
    def paths(coord: (Int, Int), visited: Set[(Int, Int)], path: List[(Int, Int)]): List[List[(Int, Int)]] = {
      val newPath = path :+ coord
      val neighbors = coords.filter(c => connected(board, coord, c) && !visited.contains(c))
      if (neighbors.isEmpty) {
        List(newPath)
      } else {
        neighbors.flatMap(c => paths(c, visited + coord, newPath))
      }
    }
    paths(coord, Set.empty[(Int, Int)], Nil)
  }

  @tailrec
  def loopHCLred(caminhos: List[List[(Int, Int)]], it: Int, size: Int): Boolean = {
    if (it == caminhos.size) {
      false
    } else {
      if (findEndred(caminhos(it), size)) {
        true
      } else {
        loopHCLred(caminhos, it + 1, size)
      }
    }
  }

  @tailrec
  def loopHCLblue(caminhos: List[List[(Int, Int)]], it: Int, size: Int): Boolean = {
    if (it == caminhos.length - 1) {
      return false
    } else {
      if (findEndblue(caminhos(it), size)) {
        true
      } else {
        loopHCLblue(caminhos, it + 1, size)
      }
    }
  }


  def findEndred(caminhos: List[(Int, Int)], size: Int): Boolean = {
    caminhos.foldLeft(false) { case (acc, (x, y)) =>
      if (y == size - 1) {
        true
      } else {
        acc
      }
    }
  }


  def findEndblue(caminhos: List[(Int, Int)], size: Int): Boolean = {
    caminhos.foldLeft(false) { case (acc, (x, y)) =>
      if (x == size - 1) {
        println("red wins")
        true
      } else {
        acc
      }
    }
  }


  def alterarBoard(board: Board, player: Cells.Cell, size: Int, rand: MyRandom): Board = {
    val newBoard = board.map(_.toList)
    val Coord(row, col) = if (player == Cells.Red) {
      getUserLoop(player, board, size)
    } else {
      randomMove(board, rand.nextInt2._2)._1
    }
    val newBoardRow = newBoard(row)
    val newBoardRowUpdated = newBoardRow.updated(col, player)
    val newBoardUpdated = newBoard.updated(row, newBoardRowUpdated)
    newBoardUpdated

  }



}
















package Hex

import Hex.FxApp.game
import Hex.boardObj.Board
import Hex.projeto.{hasContiguousLine, randomMove}

import scala.annotation.tailrec
import projetoUtilis.{alterarBoardGUI, alterarBoardTUI, aply, getContinuousPaths, loopHCLblue, loopHCLred, playerList, randomMoveCells}
import projetoUtilisTUI.{endTui, undoTUIloop}

import scala.util.control.Breaks.break

object Cells extends Enumeration {
  type Cell = Value
  val Red, Blue, Empty = Value
  val x = Int
  val y = Int
}

object boardObj {

  type Board = List[List[Cells.Cell]]
  type player = Cells.Cell
  type row = Int
  type col = Int
}



case class projeto( board:Board, rand:MyRandom, size:Int, gui:Boolean ,player :Cells.Cell){



  //---------------------------------------------- PLAY LOOP -----------------------------------------------------------

  def alterar(GUI:Boolean,pl:Cells.Cell):projeto = {

    val ranMov=randomMove(board,rand)

    GUI match {
    case true =>
      pl match {
        case  Cells.Red =>

        /*  val nb=alterarBoardGUI(board,size,Cells.Red, ranMov._2)
            val un= undoGUIloop (board,nb,size,pl,ranMov._2)
                if (!hasContiguousLine(un, size, 0)) {
                   projeto(un, ranMov._2, size, gui, Cells.Blue)
                   } else {
                      endGUI(game.player)
                    }*/

            (projeto(aply(ranMov._1, board, Cells.Red), ranMov._2, size, gui, Cells.Blue))
        case Cells.Blue =>

          /*  val nb=alterarBoardGUI(board,size,Cells.Blue, ranMov._2)
                      val un= undoGUIloop (board,nb,size,pl,ranMov._2)
                          if (!hasContiguousLine(un, size, 0)) {
                             projeto(un, ranMov._2, size, gui, Cells.Red)
                             } else {
                                endGUI(game.player)
                              }*/

             projeto(alterarBoardGUI(board, size), ranMov._2, size, gui, Cells.Red)
      }
    case false =>
      pl match {
        case Cells.Red =>
          val nb=alterarBoardTUI(board,size,Cells.Red, ranMov._2)
          val un= undoTUIloop(board,nb,size,pl,ranMov._2)
          if(!hasContiguousLine(un,size,0)){projeto (un, ranMov._2,size,gui,Cells.Blue)
          } else{
            endTui(game.player)
            break
          }
        case Cells.Blue=>
          val nb=alterarBoardTUI(board,size,Cells.Blue, ranMov._2)
          val un= undoTUIloop(board,nb,size,pl,ranMov._2)
          if (!hasContiguousLine(un, size, 0)){ projeto (un, ranMov._2,size,gui, Cells.Red)
      } else {
            endTui(game.player)
            break
          }
      }
  }
  }

  //--------------------------------------------------------------------------------------------------------------------


}



  case class Coord(row: Int, col: Int){
    def apply(x: Int, y:Int): Coord={
      Coord(x,y)
    }
  }



object projeto {


  @tailrec
  def randomMove(board: Board, rand: MyRandom): (Coord, MyRandom) = {
    val a = randomMoveCells(rand)
    val ret = (Coord(a._1._1, a._1._2), rand.nextInt2._2)
    if (board(ret._1.row)(ret._1.col) == Cells.Empty) {
      ret
    } else {
      randomMove(board, ret._2)
    }
  }
  

  def printBoard(board: Board, size: Int): Unit = {
    val header = "   " + (0 to size - 1).mkString(" ") + "\n"
    val rows = board.zipWithIndex.map { case (row, i) =>
      val cells = row.map {
        case Cells.Red => "R"
        case Cells.Blue => "B"
        case Cells.Empty => "-"
      }.mkString("|")
      val chardef = " " * i
      s"$chardef $i |$cells|"
    }
    println(header + rows.mkString("\n"))
    println("")
    val red = playerList(board, Cells.Red).mkString
    val blue = playerList(board, Cells.Blue).mkString
    println("red : " + red)
    println("blue : " + blue + ("\n"))
  }


  @tailrec
  def hasContiguousLine(board: Board, size: Int, i: Int): Boolean = {
    if (i == size - 1) {
       false
    } else {
      val caminhosred = getContinuousPaths(board, Cells.Red, (i, 0))
      val caminhosblue = getContinuousPaths(board, Cells.Blue, (0, i))
      if (loopHCLred(caminhosred, 0, size) || (loopHCLblue(caminhosblue, 0, size))) {
        true
      } else {
        hasContiguousLine(board, size, i + 1);
      }
    }
  }





}



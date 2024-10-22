package Hex

trait RandomWithState {
  def nextInt: (Int, RandomWithState)
  def nextInt(n: Int): (Int, RandomWithState)
  def randomPar(r: MyRandom): ((Int,Int), RandomWithState)
}






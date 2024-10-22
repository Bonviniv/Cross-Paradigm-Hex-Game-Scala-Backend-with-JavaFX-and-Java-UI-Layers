package Hex


case class MyRandom(seed: Long) extends RandomWithState {
  def nextInt: (Int, RandomWithState) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRandom = MyRandom(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRandom)
  }

  def nextInt2: (Int, MyRandom) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRandom = MyRandom(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRandom)
  }

  def nextInt(n: Int): (Int, RandomWithState) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRandom = MyRandom(newSeed)
    val nn = ((newSeed >>> 16).toInt) % n
    (if (nn < 0) -nn else nn, nextRandom)
  }

  def randomPar(r: MyRandom): ((Int, Int), RandomWithState) = {
    val (i1, r2) = r.nextInt(5)
    val (i2, r3) = r2.nextInt(5)
    ((i1, i2), r3)
  }

}

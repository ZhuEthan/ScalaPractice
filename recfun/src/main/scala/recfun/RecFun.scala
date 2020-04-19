package recfun

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c < 0 || c > r) -1

    if (c == 0 || c == r) 1 else (pascal(c-1, r-1) + pascal(c, r-1))

  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balance(chars: List[Char], left: Int): Boolean = {
      if (chars.isEmpty && left == 0) true
      else if (chars.head.equals(')') && left > 0) balance(chars.tail, left-1)
      else if (chars.head.equals('(')) balance(chars.tail, left+1)
      else if (chars.head.equals(')') && left == 0) false
      else balance(chars.tail, left)
    }

    balance(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money > 0 && !coins.isEmpty) {
      countChange(money, coins.tail) + countChange(money-coins.head, coins)
    } else if (money == 0) 1 else 0
  }
}

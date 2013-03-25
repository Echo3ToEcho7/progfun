package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    c match {
      case 0 => 1
      case `r` => 1
      case _ => pascal(c - 1, r - 1) + pascal(c, r - 1)
    }
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balanceHelper(chars: List[Char], count: Int): Boolean = {
      if (chars.isEmpty) {
        return count == 0
      }

      if (count < 0) {
        return false
      }

      chars.head match {
        case '(' => balanceHelper(chars.tail, count + 1)
        case ')' => balanceHelper(chars.tail, count - 1)
        case _ => balanceHelper(chars.tail, count)
      }

    }

    balanceHelper(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = ???
}

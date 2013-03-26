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
    if ((c < 0) || (r < 0)) {
      0
    } else {
      c match {
        case 0 => 1
        case `r` => 1
        case _ => pascal(c - 1, r - 1) + pascal(c, r - 1)
      }
    }
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balanceHelper(chars: List[Char], count: Int): Boolean = {
      if (chars.isEmpty) {
        count == 0
      } else if (count < 0) {
        false
      } else {
        chars.head match {
          case '(' => balanceHelper(chars.tail, count + 1)
          case ')' => balanceHelper(chars.tail, count - 1)
          case _ => balanceHelper(chars.tail, count)
        }
      }
    }

    balanceHelper(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def sortedCoins = coins.sortWith(_ < _)

    def countChangeListEmpty(moneyLeft: Int, coin: Int, numFound: Int): Int = {
      (moneyLeft - coin) match {
        case 0 => numFound + 1
        case r if r > 0 => countChangeListEmpty(r, coin, numFound)
        case _ => numFound
      }
    }

    def countChangeWithElts(moneyLeft: Int, coin: Int, coinsTail: List[Int], numFound: Int): Int = {
      (moneyLeft - coin) match {
        case 0 => numFound + 1
        case r if r > 0 => countChangeHelper(r, coin, coinsTail, numFound) + countChangeHelper(moneyLeft, coinsTail.head, coinsTail.tail, numFound)
        case _ => countChangeHelper(moneyLeft, coinsTail.head, coinsTail.tail, numFound)
      }
    }

    def countChangeHelper(moneyLeft: Int, coinsHead: Int, coinsTail: List[Int], numFound: Int): Int = {
      coinsTail.isEmpty match {
        case true => countChangeListEmpty(moneyLeft, coinsHead, numFound)
        case false => countChangeWithElts(moneyLeft, coinsHead, coinsTail, numFound)
      }
    }

    sortedCoins.isEmpty match {
      case true => 0
      case false => countChangeHelper(money, sortedCoins.head, sortedCoins.tail, 0)
    }
  }
}

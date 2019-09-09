package recfun

import scala.annotation.tailrec

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
      if (r < 0 || c < 0 || c > r)
        throw new IllegalArgumentException("Invalid coordinates: must be 0 <= c <= r")
      def _pascal(c: Int, r: Int): Int = {
        if (r < 0 || c > r)
          0
        else if (c == 0 || c == r)
          1
        else
          _pascal(c - 1, r - 1) + _pascal(c, r - 1)
      }
      _pascal(c, r)
	}
  
  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    @tailrec
    def bal(chars: List[Char], parBalance: Int): Boolean = {
      if (parBalance < 0)
        return false
      if (chars.isEmpty)
        return parBalance == 0

      val c = chars.head
      if (c == '(')
        bal(chars.tail, parBalance + 1)
      else if (c == ')')
        bal(chars.tail, parBalance - 1)
      else
        bal(chars.tail, parBalance)
    }
    bal(chars, 0)
  }


  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if (money == 0)
        1
      else if (money < 0 || coins.isEmpty)
        0
      else
        countChange(money - coins.head, coins) +
          countChange(money, coins.tail)
    }
  }

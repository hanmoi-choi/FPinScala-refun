package recfun

import common._

object Main {

  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row) {
        print(pascal(col, row) + " ")
      }
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    def isTerminalElement: Boolean = {
      c == 0 || c == r
    }

    if (isTerminalElement) {
      1
    } else {
      pascal(c - 1, r - 1) + pascal(c, r - 1)
    }
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def checkParen(c: Char): Int = c match {
      case '(' => 1
      case ')' => -1
      case _ => 0
    }

    def aux(c: Int, chars: List[Char]): Boolean =
      if (c < 0) {
        false
      }
      else if (chars.isEmpty) {
        true
      }
      else {
        aux(c + checkParen(chars.head), chars.tail)
      }

    aux(0, chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    val filteredList = coins.filter(c => c <= money)

    def aux(money:Int, coins:List[Int]):Int =
      if(money == 0) 1
      else {
        if(coins.length == 0 || money < 0) 0
        else aux(money, coins.tail) + aux(money - coins.head, coins)
      }

    aux(money, filteredList)
  }
}

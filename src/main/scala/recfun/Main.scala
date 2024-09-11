package recfun
import common._

import scala.annotation.tailrec
import scala.collection.mutable.Map

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
    if (c == 0 | c == r){
      1
    } else {
      pascal(c, r-1) + pascal(c-1, r-1)
    }
  }


  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    @tailrec
    def balanceCounter(chars: List[Char])(nb: Int): Boolean = {
      if (nb < 0)
        false
      else {
        if (chars.isEmpty) {
          nb == 0
        } else {
          if (chars.head == '(') {
            balanceCounter(chars.tail)(nb + 1)
          } else if (chars.head == ')') {
            balanceCounter(chars.tail)(nb - 1)
          } else {
            balanceCounter(chars.tail)(nb)
          }
        }
      }
    }
    balanceCounter(chars)(0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money < 0 || coins.isEmpty)
      0
    else if(money == 0)
      1
    else{
      countChange(money, coins.tail) + countChange(money - coins.head, coins)
    }
  }
}

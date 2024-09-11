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
    println("Balance")
    val unbalanced = List("())(", ")(", "(grp(vfeio)fue(fjno)", "fnfdf)")
    val balanced = List("()", "(((())))", "dkei(,fi(,k)g,j)f,j(,vfj)", "(    )", "", "fjrbknln")
    for (u <- unbalanced){
      if (balance(u.toList))
        println(u + " should be unbalanced")
    }
    for (b <- balanced)
      if (!balance(b.toList))
        println(b + " should NOT be balanced")

    println("countChange")
    val examples: List[List[List[Int]]] = List(
      List(List(1,2), List(4, 3)),
      List(List(1,2,5), List(11, 11)),
      List(List(10), List(1000, 1)),
      List(List(10), List(91, 0)),
      List(List(1,2,5,10), List(8, 7)),
      List(List(1,2,5,10), List(11,12)),
      List(List(2), List(0, 1)))

    for (example <- examples){
      if (countChange(example.tail.head.head, example.head) != example.tail.head.tail.head)
        println("money " + example.tail.head.head + " should be " + example.tail.head.tail.head + " but found " + countChange(example.tail.head.head, example.head))
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

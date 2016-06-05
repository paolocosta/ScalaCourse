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

    def pascal(c: Int, r: Int): Int =
      {
        if (c > r ) 0 else if(r==0 || c==0) 1 else (pascal(c-1, r-1) + pascal(c, r-1))
      }
  
  /**
   * Exercise 2
   */

    def balance(chars: List[Char]): Boolean = {
      @tailrec
      def verify(openParenthesisSoFar:Int, list:List[Char]):Boolean =
      {
          def calculateParenthesis(char:Char):Int = {
            if (char == '(') 1 else if (char == ')') -1 else 0
          }
          if(openParenthesisSoFar < 0)
              false
          else if(list.isEmpty)
            openParenthesisSoFar == 0
          else {
            verify(openParenthesisSoFar + calculateParenthesis(list.head), list.tail)
          }
      }
      verify(0,chars)
    }
  
  /**
   * Exercise 3
   */

    def countChange(money: Int, coins: List[Int]): Int = {
      if(money == 0)
        1
      else
        if(money < 0 || coins.isEmpty) 0
      else
        countChange(money-coins.head, coins) + countChange(money, coins.tail)
    }
  }

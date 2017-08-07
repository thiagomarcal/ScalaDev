package recfun

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
      if (c == 0 || c == r) 1 else pascal(c - 1, r - 1) + pascal( c , r - 1)
    }
  
  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    var currentPos = 0

    def loop(init : Int, initRight : Int, initLeft : Int) : Boolean = {

      var open = init
      var left : Int = initLeft
      var right : Int = initRight

      if (chars.length != currentPos) {
        if (chars(currentPos) == '(') {
          open += 1
          left += 1
        }
        if (chars(currentPos) == ')') {
          right += 1
          if (open > 0) open -= 1
        }
        currentPos += 1
        loop(open, right, left)
      } else {
        left > 0 && right > 0 && open == 0
      }
    }
    loop(0, 0, 0)
  }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      def count(m: Int, c: List[Int]) : Int = {
        if (c.isEmpty) 0
        else if (m - c.head == 0) 1
        else if (m - c.head < 0) 0
        else countChange(m - c.head, c) + countChange(m, c.tail)
      }
      count(money, coins.sorted)
    }
  }

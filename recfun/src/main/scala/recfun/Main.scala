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
    if (c > r) 
      0
    else if (c == 0 || r == 0) 
      1
    else
      pascal(c-1, r-1) + pascal(c, r-1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = { 
    def fn(chars: List[Char], b: Int):Boolean = {
      if (chars.isEmpty)
        return b == 0
      else
        chars.head match {
          case '(' => fn(chars.tail, b+1) 
          case ')' => b > 0 && fn(chars.tail, b-1)
          case _ => fn(chars.tail, b)
        }
    }
    fn(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money < 0 || coins.isEmpty) 0
    else if (money == 0) 1
    else
      countChange(money, coins.tail) + 
      countChange(money - coins.head, coins)
  }
}

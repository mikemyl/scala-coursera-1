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
    def isInvalidTrianglePosition = {
      (c < 0) || (r < 0) || (c > r)
    }

    if ((c, r) == (0, 0))
      1
    else if (isInvalidTrianglePosition)
      0
    else
      pascal(c - 1, r - 1) + pascal(c, r - 1)

  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def auxBalance(chars: List[Char], opening: Int): Boolean = {
      if (chars.isEmpty && (opening == 0))
        true
      else if (opening < 0)
        false
      else {
        (chars.head: Char) match {
          case ')' => auxBalance(chars.tail, opening - 1)
          case '(' => auxBalance(chars.tail, opening + 1)
          case _ => auxBalance(chars.tail, opening)
        }
      }
    }

    auxBalance(chars, 0)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0)
      1
    else if (coins.nonEmpty && money > 0)
      countChange(money, coins.tail) + countChange(money - coins.head, coins)
    else
      0
  }
}
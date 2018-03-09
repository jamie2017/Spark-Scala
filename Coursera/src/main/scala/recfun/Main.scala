package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }

    println("Parentheses Balancing")
    assert(balance("(if (zero? x) max (/ 1 x))".toList))
    assert(balance("I told him (that it’s not (yet) done). (But he wasn’t listening)".toList))
    assert(!balance(":-)".toList))
    assert(!balance("())(".toList))

  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = {
      if (c == 0 || c == r) 1
      else pascal(c - 1, r - 1) + pascal(c, r- 1)
    }
  
  /**
   * Exercise 2
    *
    * chars.isEmpty: Boolean returns whether a list is empty
    * chars.head: Char returns the first element of the list
    * chars.tail: List[Char] returns the list without the first element
   */
    def balance(chars: List[Char]): Boolean = {
      def helper(chars: List[Char],numOpens: Int): Boolean = {
        if(chars.isEmpty) numOpens == 0
        else
          if (chars.head == '(') helper(chars.tail, numOpens + 1)
          else
            if (chars.head == ')') numOpens > 0 && helper(chars.tail,numOpens - 1)
            else helper(chars.tail,numOpens)
      }
      helper(chars,0)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if (coins.isEmpty) 0
      else if (money - coins.head == 0) 1
      else if (money - coins.head < 0) 0
      else countChange(money - coins.head, coins) + countChange(money, coins.tail)
    }
  }

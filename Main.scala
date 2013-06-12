
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
   * pascal triangle
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r)
      1
    else
      pascal(c, r-1)  + pascal(c-1, r-1)
  }

  /**
   * Check if a list if chars contains balanced Parens
   * ((s^n)-1) *6
   */
  def balance(chars: List[Char]): Boolean = {
    def doBanlance(chars: List[Char], rightParenNumber: Int): Boolean = {
      if (chars.length == 0) {
	    	rightParenNumber == 0
	    } else {
	       val temp = 
		      chars.head match {
			      case '(' => rightParenNumber + 1
			      case ')' => rightParenNumber - 1
			      case _ => rightParenNumber
	      	}
	      if (temp >= 0)
	    	  doBanlance(chars.tail, temp)
	      else
	    	  false
	    }
    }
    doBanlance(chars, 0)
  }

  /**
   * total ways to make to represents certain amount of money by given coins
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0)
      1
    else if (money < 0 || coins.length == 0)
      0
    else {
      countChange(money - coins.head, coins) + countChange(money, coins.tail)  
    }
  }
}

package templemore.luhnybin

object LuhnCheck {
  def hasValidLuhn(digits: Seq[Int]) = {
    def sumOfDigits = digits.foldRight((0, false)) { (digit, sum) =>
      val d = if ( sum._2 ) digit * 2 else digit
      (sum._1 + (if (d > 9) ((d / 10) + (d % 10)) else d), !sum._2)
    }._1

    require(digits.length >= 2)
    require(digits.forall(d => d >= 0 && d <= 9))
   sumOfDigits % 10 == 0
  }
}
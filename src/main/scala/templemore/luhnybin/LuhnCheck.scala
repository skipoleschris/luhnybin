package templemore.luhnybin

object LuhnCheck {
  def hasValidLuhn(digits: Seq[Int]) = {
    def doubleAndSplit(i: Int) = (i * 2) match {
      case d if ( d > 9 ) => List(d / 10, d % 10)
      case d => List(d)
    }
    
    require(digits.length >= 2)
    require(digits.forall(d => d >= 0 && d <= 9))

    val splitDigits = partitionOnIndex(digits.toList.reverse, (_ % 2 == 0))
    (splitDigits._1.sum + (splitDigits._2.flatMap(doubleAndSplit).sum)) % 10 == 0
  }
  
  private def partitionOnIndex[T](xs: List[T], f: (Int) => Boolean): Pair[List[T], List[T]] = {
    val split = xs.zipWithIndex.partition(item => f(item._2))
    (split._1.unzip._1, split._2.unzip._1)
  }
}
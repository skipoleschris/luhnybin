package templemore.luhnybin


object CardNumberMasker {
  def mask(inputString: String): String = mask(inputString.toList).mkString("")

  def mask(inputString: List[Char]): List[Char] = inputString match {
  	case Nil => Nil
  	case x :: xs if x.isDigit => {
  		val maskResult = maskCardNumber(MaskCandidate(x), xs)
  		maskResult._1 ++ mask(maskResult._2)
  	}
  	case x :: xs => x :: mask(xs)
  }

  private def maskCardNumber(candidate: MaskCandidate, remainder: List[Char]): Pair[List[Char], List[Char]] = {
  	remainder match {
  		case x :: xs if candidate.accept(x) => maskCardNumber(candidate.add(x), xs)
  		case _ => {
  			val mask = candidate.applyMask
  			(mask._1.reverse, mask._2.reverse ++ remainder)
  		}
  	}
  }
}
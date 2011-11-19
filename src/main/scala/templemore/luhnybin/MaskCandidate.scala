package templemore.luhnybin

case class MaskCandidate(characters: List[Char] = List(), digitCount: Int, startIndex: Int) {

  private val MinDigits = 14
  val MaxDigits = 16
  type MaybeMask = Option[List[Char]]

  def buildCriteria: MaskCriteria = {
    def skipCount = if ( digitCount <= MinDigits) characters.length - 1 else 0
    def reduceAndCheck(masked: MaybeMask, length: Int) = masked.orElse(
      reduceToLength(length) flatMap { chars => if ( isCardNumber(chars) ) Some(chars) else None }
    )

    if ( digitCount < MinDigits ) MaskCriteria(startIndex, 0, skipCount)
    else (MaxDigits to MinDigits by -1).foldLeft[MaybeMask](None)(reduceAndCheck)
                                       .map(result => MaskCriteria(startIndex, result.length, skipCount))
                                       .getOrElse(MaskCriteria(startIndex, 0, skipCount))
  }

	private def reduceToLength(length: Int) = {
		if ( digitCount < length ) None
		else if ( digitCount == length ) Some(characters)
		else {
			val dropCount = digitCount - length
			var dropped = 0
			Some(characters.dropWhile { ch =>
			  if ( ch.isDigit ) dropped = dropped + 1
			  dropped <= dropCount
			})
		}
	}

	private def isCardNumber(chars: List[Char]) =
		LuhnCheck.hasValidLuhn(chars.filter(_.isDigit).reverse.map(_.getNumericValue))
}

object MaskCandidate {
  def apply(input: List[Char], startIndex: Int) = {
    def accept(ch: Char, count: Int) = count < 16 && (ch.isDigit || ch == ' ' || ch == '-')

    require(!input.isEmpty && input.head.isDigit)
    var count = 0
    new MaskCandidate(input.takeWhile { ch =>
      val result = accept(ch, count)
      if ( ch.isDigit && result ) count = count + 1
      result
    }.reverse, count, startIndex)
  }
}

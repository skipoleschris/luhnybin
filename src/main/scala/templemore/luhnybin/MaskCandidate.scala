package templemore.luhnybin

case class MaskCandidate(characters: List[Char] = List(), digitCount: Int, startIndex: Int, locked: Boolean = false) {

	def add(ch: Char) = {
    def accept(ch: Char) = digitCount < 16 && !locked && (ch.isDigit || ch == ' ' || ch == '-')

    if ( accept(ch) ) new MaskCandidate(ch :: characters, if ( ch.isDigit ) digitCount + 1 else digitCount, startIndex)
    else if ( !locked ) copy(locked = true)
    else this
  }

  def buildCriteria: MaskCriteria = {
    def skipCount = if ( digitCount <= 14) characters.length - 1 else 0
    def reduceAndCheck(length: Int): Option[List[Char]] = reduceToLength(length).flatMap { chars =>
      if ( isCardNumber(chars) ) Some(chars) else None
    }

    if ( digitCount < 14 ) MaskCriteria(startIndex, 0, skipCount)
    else {
      val masked = reduceAndCheck(16).orElse(reduceAndCheck(15)).orElse(reduceAndCheck(14))
      masked.map(result => MaskCriteria(startIndex, result.length, skipCount))
            .getOrElse(MaskCriteria(startIndex, 0, skipCount))
    }
  }

	private def reduceToLength(length: Int): Option[List[Char]] = {
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

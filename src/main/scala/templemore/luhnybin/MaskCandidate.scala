package templemore.luhnybin

import scala.None

case class MaskCandidate(characters: List[Char] = List(), digitCount: Int, startIndex: Int, locked: Boolean = false) {

  private val MinDigits = 14
  private val MaxDigits = 16
  type MaybeMask = Option[List[Char]]

	def add(ch: Char) = {
    def accept(ch: Char) = digitCount < MaxDigits && !locked && (ch.isDigit || ch == ' ' || ch == '-')

    if ( accept(ch) ) new MaskCandidate(ch :: characters, if ( ch.isDigit ) digitCount + 1 else digitCount, startIndex)
    else if ( !locked ) copy(locked = true)
    else this
  }

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

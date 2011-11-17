package templemore.luhnybin

case class MaskCandidate(characters: List[Char] = List(), digitCount: Int) {
	def accept(ch: Char) = digitCount < 16 && (ch.isDigit || ch == ' ' || ch == '-')

	def add(ch: Char) = new MaskCandidate(ch :: characters, if ( ch.isDigit ) digitCount + 1 else digitCount)

	def applyMask: Pair[List[Char], List[Char]] = {
  	  def reduceAndMask(length: Int): Option[List[Char]] = reduceToLength(length).flatMap(chars => if ( isCardNumber(chars) ) Some(chars) else None)
      def mask(chars: List[Char]) = chars.map(d => if ( d.isDigit) 'X' else d)

	  if ( digitCount < 14 ) (characters, Nil)
	  else {
	  	val masked = reduceAndMask(16).orElse(reduceAndMask(15)).orElse(reduceAndMask(14))
		masked.map(result => (mask(result), characters.drop(result.length))).getOrElse((characters.last :: Nil, characters.dropRight(1)))
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

object MaskCandidate {
	def apply(firstDigit: Char) = {
		require(firstDigit.isDigit)
		new MaskCandidate(firstDigit :: Nil, 1)
	}
}

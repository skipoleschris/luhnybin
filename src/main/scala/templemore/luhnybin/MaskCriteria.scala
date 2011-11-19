package templemore.luhnybin

case class MaskCriteria(fromIndex: Int, noOfCharacters: Int, skipCount: Int) {
  def isMasked = noOfCharacters > 0

  def toIndex = fromIndex + noOfCharacters

  def apply(characters: List[Char]) = {
    def maskDigits(chars: List[Char]) = chars map (ch => if (ch.isDigit) 'X' else ch)

    require(characters.length >= toIndex)
    (characters slice (0, fromIndex)) ++
    maskDigits(characters slice (fromIndex, toIndex)) ++
    (characters slice (toIndex, characters.length))
  }
}
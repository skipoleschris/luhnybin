package templemore.luhnybin


case class MaskCriteria(fromIndex: Int, noOfCharacters: Int, skipCount: Int) {
  def isMasked = noOfCharacters > 0
  def toIndex = fromIndex + noOfCharacters
}
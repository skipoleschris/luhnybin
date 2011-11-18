package templemore.luhnybin

import annotation.tailrec


object CardNumberMasker {
  def mask(inputString: String): String = mask(inputString.toList)

  def mask(inputString: List[Char]): String = {
    val masks = buildMaskCriteria(inputString, 0, List())
    masks.foldLeft(inputString) { (s, criteria) => criteria(s) }.mkString("")
  }

  @tailrec
  private def buildMaskCriteria(inputString: List[Char],
                        index: Int = 0,
                        masks: List[MaskCriteria] = List()): List[MaskCriteria] = {
    inputString match {
      case Nil => masks
      case x :: xs if x.isDigit => {
        val maskResult = MaskCriteria(inputString, index)
        buildMaskCriteria(
          inputString.drop(maskResult.skipCount + 1),
          index + 1 + maskResult.skipCount,
          if ( maskResult.isMasked ) maskResult :: masks else masks)
      }
      case x :: xs => buildMaskCriteria(xs, index + 1, masks)
    }
  }
}
package templemore.luhnybin

import annotation.tailrec

object CardNumberMasker {
  def mask(inputString: String) = {
    val characters = inputString.toList
    buildMaskCriteria(characters).foldLeft(characters) { (s, criteria) => criteria(s) }.mkString("")
  }

  @tailrec
  private def buildMaskCriteria(inputString: List[Char],
                                 index: Int = 0,
                                 masks: List[MaskCriteria] = List()): List[MaskCriteria] = {
    val remainder = inputString.dropWhile(!_.isDigit)
    if ( remainder.isEmpty ) masks
    else {
      val startIndex = index + inputString.length - remainder.length
      val maskResult = MaskCriteria(remainder, startIndex)
      buildMaskCriteria(remainder.drop(maskResult.skipCount + 1),
                        startIndex + 1 + maskResult.skipCount,
                        if ( maskResult.isMasked ) maskResult :: masks else masks)
    }
  }
}
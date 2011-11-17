package templemore.luhnybin

import annotation.tailrec


object CardNumberMasker {
  def mask(inputString: String): String = mask(inputString.toList)

  def mask(inputString: List[Char]): String = {
    def maskDigits(chars: List[Char]) = chars.map(ch => if ( ch.isDigit) 'X' else ch)

    val masks = buildMaskCriteria(inputString, 0, List())
    masks.foldLeft(inputString) { (s, criteria) =>
      s.slice(0, criteria.fromIndex) ++ 
      maskDigits(s.slice(criteria.fromIndex, criteria.toIndex)) ++
      s.slice(criteria.toIndex, s.length)
    }.mkString("")
  }

  @tailrec
  def buildMaskCriteria(inputString: List[Char],
                        index: Int = 0,
                        masks: List[MaskCriteria] = List()): List[MaskCriteria] = {
    inputString match {
      case Nil => masks
      case x :: xs if x.isDigit => {
        val maskResult = buildCriteria(MaskCandidate(x, index), xs)
        buildMaskCriteria(
          inputString.drop(maskResult.skipCount + 1),
          index + 1 + maskResult.skipCount,
          if ( maskResult.isMasked ) maskResult :: masks else masks)
      }
      case x :: xs => buildMaskCriteria(xs, index + 1, masks)
    }
  }

  @tailrec
  private def buildCriteria(candidate: MaskCandidate, remainder: List[Char]): MaskCriteria = remainder match {
    case x :: xs if candidate.accept(x) => buildCriteria(candidate.add(x), xs)
    case _ => candidate.determineMaskCount
  }
}
package templemore.luhnybin

import org.specs2.Specification


class MaskCandidateSpec extends Specification { def is =

  "Specification for the Mask Candidate class"                       ^
                                                                     endp^
  "Adding to a Mask Candidate should"                                ^
    "allow staring with a single digit"                              ! newCandidate^
    "allow adding an additional digit"                               ! addDigit^
    "allow dgging a space character"                                 ! addSpace^ 
    "allow adding a dash character"                                  ! addDash^
    "allow adding up to 16 digits"                                   ! addDigits^
    "lock the candidate when adding a non-valid character"           ! lockOnNonDigit^
    "lock the candidate when adding a 17th digit"                    ! lockOnMaxLength^
    "should not modify an already locked candidate"                  ! lockedCanNotBeModified^
                                                                     endp^
  "Building a mask criteria should"                                  ^
    "Return no mask and a skip if less than 14 digits"               ! notLongEnoughToBeACardNumber^
    "Return no mask and no skip if 16 digits and not a card"         ! notACard16Digits^
    "Return no mask and no skip if 15 digits and not a card"         ! notACard15Digits^
    "Return no mask and a skip if 14 digits and not a card"          ! notACard14Digits^
    "Return mask and no skip if 16 digits and a card"                ! card16Digits^
    "Return mask and no skip if 16 digits and 15 digit card"         ! card15of16Digits^
    "Return mask and no skip if 16 digits and 14 digit card"         ! card14of16Digits^
    "Return mask and no skip if 15 digits and a card"                ! card15Digits^
    "Return mask and no skip if 15 digits and 14 digit card"         ! card14of15Digits^
    "Return mask and skip if 14 digits and a card"                   ! card14Digits^
                                                                     end

  def newCandidate =
    MaskCandidate('1' :: Nil, 1, 0) must_==  MaskCandidate('1' :: Nil, 1, 0, false)
  
  def addDigit = 
    MaskCandidate('1' :: Nil, 1, 0).add('2') must_== MaskCandidate('2' :: '1' :: Nil, 2, 0, false)
  
  def addSpace =
    MaskCandidate('1' :: Nil, 1, 0).add(' ') must_==  MaskCandidate(' ' :: '1' :: Nil, 1, 0, false)

  def addDash =
    MaskCandidate('1' :: Nil, 1, 0).add('-') must_==  MaskCandidate('-' :: '1' :: Nil, 1, 0, false)

  def addDigits = {
    val candidate = MaskCandidate('1' :: Nil, 1, 0)
    val result = "234 5678 9012 3456".toList.foldLeft(candidate) { (c, ch) => c.add(ch) }
    result must_== MaskCandidate("6543 2109 8765 4321".toList, 16, 0, false)
  }
  
  def lockOnNonDigit = MaskCandidate('1' :: Nil, 1, 0).add('a').locked must_== true

  def lockOnMaxLength = {
    val candidate = MaskCandidate('1' :: Nil, 1, 0)
    val intermediate = "234 5678 9012 3456".toList.foldLeft(candidate) { (c, ch) => c.add(ch) }
    val result = intermediate.add('3')
    (result.digitCount must_== 16) and
    (result.locked must_== true)
  }

  def lockedCanNotBeModified = {
    val candidate = MaskCandidate('1' :: Nil, 1, 0).add('a')
    val result = candidate.add('2')
    result must be(candidate)
  }

  def notLongEnoughToBeACardNumber =
    MaskCandidate("1234-5678".toList, 8, 0, true).buildCriteria must_==  MaskCriteria(0, 0, 8)

  def notACard16Digits =
    MaskCandidate("4111-1111-1111-4119".toList, 16, 5, false).buildCriteria must_==  MaskCriteria(5, 0, 0)

  def notACard15Digits =
    MaskCandidate("4111-1111-1111411".toList, 15, 0, true).buildCriteria must_==  MaskCriteria(0, 0, 0)

  def notACard14Digits =
    MaskCandidate("4111-1111-111141".toList, 14, 0, true).buildCriteria must_==  MaskCriteria(0, 0, 15)

  def card16Digits =
    MaskCandidate("1111-1111-1111-1114".toList, 16, 0, true).buildCriteria must_==  MaskCriteria(0, 19, 0)

  def card15of16Digits =
    MaskCandidate("7611-1111-1111-1114".toList, 16, 0, true).buildCriteria must_==  MaskCriteria(0, 18, 0)

  def card14of16Digits =
    MaskCandidate("3641-1111-1111-1114".toList, 16, 0, true).buildCriteria must_==  MaskCriteria(0, 17, 0)

  def card15Digits =
    MaskCandidate("611-1111-1111-1114".toList, 15, 0, true).buildCriteria must_==  MaskCriteria(0, 18, 0)

  def card14of15Digits =
    MaskCandidate("741-1111-1111-1114".toList, 15, 0, true).buildCriteria must_==  MaskCriteria(0, 17, 0)

  def card14Digits =
    MaskCandidate("41-1111-1111-1114".toList, 14, 0, true).buildCriteria must_==  MaskCriteria(0, 17, 16)
}
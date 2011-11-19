package templemore.luhnybin

import org.specs2.Specification


class MaskCandidateSpec extends Specification { def is =

  "Specification for the Mask Candidate class"                       ^
                                                                     endp^
  "Building a mask criteria should"                                  ^
    "return no mask and a skip if less than 14 digits"               ! notLongEnoughToBeACardNumber^
    "return no mask and no skip if 16 digits and not a card"         ! notACard16Digits^
    "return no mask and no skip if 15 digits and not a card"         ! notACard15Digits^
    "return no mask and a skip if 14 digits and not a card"          ! notACard14Digits^
    "return mask and no skip if 16 digits and a card"                ! card16Digits^
    "return mask and no skip if 16 digits and 15 digit card"         ! card15of16Digits^
    "return mask and no skip if 16 digits and 14 digit card"         ! card14of16Digits^
    "return mask and no skip if 15 digits and a card"                ! card15Digits^
    "return mask and no skip if 15 digits and 14 digit card"         ! card14of15Digits^
    "return mask and skip if 14 digits and a card"                   ! card14Digits^
                                                                     endp^
  "Creating a new mask candidate should"                             ^
    "return a mask candidate for a string of just digits"            ! digitsOnly^
    "return a mask candidate for a string with spaces and hyphens"   ! digitsSpacesAndHypnens^
    "return a mask candidate for a string ending with a chararter"   ! characterTerminated^
    "return a mask candidate for a long digit string"                ! longDigitString^
    "error if supplied an empty list"                                ! emptyList^
    "error if supplied a list that doesn't start with a digit"       ! noDigitAtStart^
                                                                     end

  def notLongEnoughToBeACardNumber =
    MaskCandidate("1234-5678".toList, 8, 0).buildCriteria must_==  MaskCriteria(0, 0, 8)

  def notACard16Digits =
    MaskCandidate("4111-1111-1111-4119".toList, 16, 5).buildCriteria must_==  MaskCriteria(5, 0, 0)

  def notACard15Digits =
    MaskCandidate("4111-1111-1111411".toList, 15, 0).buildCriteria must_==  MaskCriteria(0, 0, 0)

  def notACard14Digits =
    MaskCandidate("4111-1111-111141".toList, 14, 0).buildCriteria must_==  MaskCriteria(0, 0, 15)

  def card16Digits =
    MaskCandidate("1111-1111-1111-1114".toList, 16, 0).buildCriteria must_==  MaskCriteria(0, 19, 0)

  def card15of16Digits =
    MaskCandidate("7611-1111-1111-1114".toList, 16, 0).buildCriteria must_==  MaskCriteria(0, 18, 0)

  def card14of16Digits =
    MaskCandidate("3641-1111-1111-1114".toList, 16, 0).buildCriteria must_==  MaskCriteria(0, 17, 0)

  def card15Digits =
    MaskCandidate("611-1111-1111-1114".toList, 15, 0).buildCriteria must_==  MaskCriteria(0, 18, 0)

  def card14of15Digits =
    MaskCandidate("741-1111-1111-1114".toList, 15, 0).buildCriteria must_==  MaskCriteria(0, 17, 0)

  def card14Digits =
    MaskCandidate("41-1111-1111-1114".toList, 14, 0).buildCriteria must_==  MaskCriteria(0, 17, 16)

  def digitsOnly =
    MaskCandidate("1234567".toList, 0) must_== MaskCandidate("7654321".toList, 7, 0)

  def digitsSpacesAndHypnens =
    MaskCandidate("4111 1111-1111 1111".toList, 5) must_== MaskCandidate("1111 1111-1111 1114".toList, 16, 5)

  def characterTerminated =
    MaskCandidate("123456ggg".toList, 0) must_== MaskCandidate("654321".toList, 6, 0)

  def longDigitString =
    MaskCandidate("411111111111111122223333".toList, 0) must_== MaskCandidate("1111111111111114".toList, 16, 0)

  def emptyList = MaskCandidate(List(), 0) must throwAn[Exception]
  def noDigitAtStart = MaskCandidate("foo4111111111111111".toList, 0) must throwAn[Exception]
}
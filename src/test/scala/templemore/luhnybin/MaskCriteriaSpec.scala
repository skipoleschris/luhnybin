package templemore.luhnybin

import org.specs2.Specification

class MaskCriteriaSpec extends Specification { def is =

  "Specification for the Mask Criteria class"                        ^
                                                                     endp^
  "The Mask Criteria should"                                         ^
    "report that a non-zero number of characters should be masked"   ! mask^
    "report that a zero number of characters should not be masked"   ! doNotMask^
    "report the correct to index value for a given length"           ! fromIndex^
                                                                     endp^
  "Applying a mask criteria to a string should"                      ^
    "mask all digits in the string"                                  ! allDigits^
    "mask digits at the start of the string"                         ! startDigits^
    "mask digits in the center of the string"                        ! middleDigits^
    "mask digits at the end of the string"                           ! endDigits^
    "not mask if there are no digits"                                ! noDigits^
    "error if the string is shorter than the mask criteria"          ! invalidString^
                                                                     end
  
  def mask = MaskCriteria(0, 14, 0).isMasked must_== true
  def doNotMask = MaskCriteria(0, 0, 0).isMasked must_== false
  def fromIndex = MaskCriteria(10, 15, 0).toIndex must_== 25

  def allDigits = MaskCriteria(0, 10, 0).apply("1234567890".toList) must_== "XXXXXXXXXX".toList
  def startDigits = MaskCriteria(0, 5, 0).apply("1234567890".toList) must_== "XXXXX67890".toList
  def middleDigits = MaskCriteria(3, 5, 0).apply("1234567890".toList) must_== "123XXXXX90".toList
  def endDigits = MaskCriteria(5, 5, 0).apply("1234567890".toList) must_== "12345XXXXX".toList
  def noDigits = MaskCriteria(0, 10, 0).apply("abcdefghij".toList) must_== "abcdefghij".toList
  def invalidString = MaskCriteria(3, 5, 0).apply("hello".toList) must throwAn[Exception]
}
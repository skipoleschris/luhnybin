package templemore.luhnybin

import org.specs2.Specification

class LuhnCheckSpec extends Specification { def is =

  "Specification for the Luhn Check"                                 ^
                                                                     endp^
  "The Luhn Check should"                                            ^
    "pass when given a valid even length number"                     ! validLuhnEven^
    "fail when given an invalid even length number"                  ! invalidLuhnEven^
    "pass when given a valid odd length number"                      ! validLuhnOdd^
    "fail when given an invalid odd length number"                   ! invalidLuhnOdd^
    "error when given less than 2 digits"                            ! tooFewDigits^
    "error when given a digit greater than 9"                        ! digitTooBig^
    "error when given a negative value digit"                        ! digitTooSmall^
                                                                     end

  def validLuhnEven = LuhnCheck.hasValidLuhn(List(5, 6, 7, 8)) must_== true
  def invalidLuhnEven = LuhnCheck.hasValidLuhn(List(6, 7, 8, 9)) must_== false
  def validLuhnOdd = LuhnCheck.hasValidLuhn(List(3, 4, 5, 6, 1)) must_== true
  def invalidLuhnOdd = LuhnCheck.hasValidLuhn(List(5, 6, 7, 8, 9)) must_== false
  def tooFewDigits = LuhnCheck.hasValidLuhn(List(0)) must throwAn[Exception]
  def digitTooBig = LuhnCheck.hasValidLuhn(List(0, 6, 10, 4)) must throwAn[Exception]
  def digitTooSmall = LuhnCheck.hasValidLuhn(List(0, 6, -1, 4)) must throwAn[Exception]
}
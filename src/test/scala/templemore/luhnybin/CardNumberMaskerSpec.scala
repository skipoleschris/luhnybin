package templemore.luhnybin

import org.specs2.Specification


class CardNumberMaskerSpec extends Specification { def is =

  "Specification for the Card Number Masker"                         ^
                                                                     endp^
  "The Card Number Masker should"                                    ^
    "not change a string containing no digits"                       ! noDigitsToMask^
    "not change a string containing too few digits"                  ! tooFewDigits^
    "not change a string that doesn't contain a card number"         ! notACardNumber^
    "should mask a 16 digit card number"                             ! mask16^
    "should mask a 15 digit card number"                             ! mask15^
    "should mask a 14 digit card number"                             ! mask14^
    "should mask a card number containing spaces and hyphens"        ! maskSpaced^
    "should mask overlapping card numbers"                           ! maskOverlapping^
    "should mask a card number at the start of the string"           ! maskAtStart^
    "should mask a card number at the end of the string"             ! maskAtEnd^
    "should mask a card number embedded inside digits"               ! maskEmbedded^
                                                                     end

  def noDigitsToMask = 
    CardNumberMasker.mask("There are no digits here") must_== "There are no digits here"

  def tooFewDigits =
    CardNumberMasker.mask("23-45-56 There were 845673 errors from 124-245-37-4") must_==
      "23-45-56 There were 845673 errors from 124-245-37-4"

  def notACardNumber =
    CardNumberMasker.mask("This number: 4111111111111116, looks like a card but it isn't") must_==
      "This number: 4111111111111116, looks like a card but it isn't"

  def mask16 =
    CardNumberMasker.mask("This number: 4111111111111111, is a card number") must_==
      "This number: XXXXXXXXXXXXXXXX, is a card number"

  def mask15 =
    CardNumberMasker.mask("This number: 411111111111116, is a card number") must_==
      "This number: XXXXXXXXXXXXXXX, is a card number"

  def mask14 =
    CardNumberMasker.mask("This number: 41111111111114, is a card number") must_==
      "This number: XXXXXXXXXXXXXX, is a card number"

  def maskSpaced =
    CardNumberMasker.mask("This number: 4111 1111-1111 1111, is a card number") must_==
      "This number: XXXX XXXX-XXXX XXXX, is a card number"

  def maskOverlapping =
    CardNumberMasker.mask("This number: 4111-1111-1111-1111-1111-1117, has overlapping card numbers") must_==
      "This number: XXXX-XXXX-XXXX-XXXX-XXXX-XXXX, has overlapping card numbers"

  def maskAtStart =
    CardNumberMasker.mask("4111111111111111, is a card number") must_==
      "XXXXXXXXXXXXXXXX, is a card number"

  def maskAtEnd =
    CardNumberMasker.mask("The card number is: 4111111111111111") must_==
      "The card number is: XXXXXXXXXXXXXXXX"

  def maskEmbedded =
    CardNumberMasker.mask("9875610591081018250321") must_== "987XXXXXXXXXXXXXXXX321"
}
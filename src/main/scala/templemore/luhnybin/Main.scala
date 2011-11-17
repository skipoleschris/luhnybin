package templemore.luhnybin

import io.Source

object Main extends App {
  Source.stdin.getLines().foreach { line =>
    Console.println(CardNumberMasker.mask(line))
  }
}
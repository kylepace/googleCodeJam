import org.scalatest.{FlatSpec, Matchers}

class T9TextingSpec extends FlatSpec with Matchers {
  "convert" should "convert numbers to text" in {
    val first = "44 444"
    val second = "999337777"
    val third = "333666 6660 022 2777"
    val fourth = "4433555 555666096667775553"

    T9Texting.convert(first) shouldBe "hi"
    T9Texting.convert(second) shouldBe "yes"
    T9Texting.convert(third) shouldBe "foo  bar"
    T9Texting.convert(fourth) shouldBe "hello world"
  }

  "t9 map" should "return a for 2" in {
    T9Texting.codeMap get "2" shouldBe Some("a")
  }

  it should "return h for 44" in {
    T9Texting.codeMap get "44" shouldBe Some("h")
  }

  it should "return space for 0" in {
    T9Texting.codeMap get "0" shouldBe Some(" ")
  }

  it should "return correct chars for 7s" in {
    T9Texting.codeMap get "7" shouldBe Some("p")
    T9Texting.codeMap get "77" shouldBe Some("q")
    T9Texting.codeMap get "777" shouldBe Some("r")
    T9Texting.codeMap get "7777" shouldBe Some("s")
  }

  it should "return correct chars for 9s" in {
    T9Texting.codeMap get "9" shouldBe Some("w")
    T9Texting.codeMap get "99" shouldBe Some("x")
    T9Texting.codeMap get "999" shouldBe Some("y")
    T9Texting.codeMap get "9999" shouldBe Some("z")
  }

  "t9 parse" should "return one code per one input" in {
    T9Texting.parseCodes("44") shouldBe List("44")
    T9Texting.parseCodes("7777") shouldBe List("7777")
  }

  it should "return two codes when split by a space" in {
    T9Texting.parseCodes("44 4") shouldBe List("44", "4")
  }

  it should "return three codes with one space" in {
    T9Texting.parseCodes("44 4555") shouldBe List("44", "4", "555")
  }

  it should "return three codes in one string" in {
    T9Texting.parseCodes("999337777") shouldBe List("999", "33", "7777")
  }
}

import org.scalatest.{FlatSpec, Matchers}

class ReverseWordSpec extends FlatSpec with Matchers {
  "parseFile" should "return correct lines" in {
    val rows = ReverseWords.parseFile("reverseword_small_file.txt")
    rows(0) shouldBe "this is a test"
    rows(1) shouldBe "foobar"
    rows(2) shouldBe "all your base"
    rows(3) shouldBe "class"
    rows(4) shouldBe "pony along"
  }

  "run" should "return reversed sentence" in {
    val output = ReverseWords.run(List("this is a test", "foobar", "all your base"))
    output(0) shouldBe "test a is this"
    output(1) shouldBe "foobar"
    output(2) shouldBe "base your all"
  }

  "write file" should "write small file" in {
    val rows = ReverseWords.parseFile("reverseword_small_file.txt")
    val reversedWords = ReverseWords.run(rows)
    ReverseWords.writeFile("/Users/kpace/Documents/Code/reverseword_small_file_solution.txt", reversedWords)
  }

  it should "write large file" in {
    val rows = ReverseWords.parseFile("reverseword_large_file.txt")
    val reversedWords = ReverseWords.run(rows)
    ReverseWords.writeFile("/Users/kpace/Documents/Code/reverseword_large_file_solution.txt", reversedWords)
  }
}

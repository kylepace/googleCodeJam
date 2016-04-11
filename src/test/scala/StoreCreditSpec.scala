import org.scalatest._

class StoreCreditSpec extends FlatSpec with Matchers {
  "findPricePair" should "return proper indexes in three item list" in {
    val credit = 4
    val pair = StoreCredit.findPricePair(credit, List(1, 2, 3))
    assert(pair._1 == 1)
    assert(pair._2 == 3)
  }

  it should "return 2 3 with 100 credit" in {
    val credit = 100
    val pair = StoreCredit.findPricePair(credit, List(5, 75, 25))
    assert(pair._1 == 2)
    assert(pair._2 == 3)
  }

  it should "return 1 4 with 200 credit" in {
    val credit = 200
    val pair = StoreCredit.findPricePair(credit, List(150, 24, 79, 50, 88, 345, 3))
    assert(pair._1 == 1)
    assert(pair._2 == 4)
  }

  it should "return 4 5 with 8 credit" in {
    val credit = 8
    val pair = StoreCredit.findPricePair(credit, List(2, 1, 9, 4, 4, 56, 90, 3))
    assert(pair._1 == 4)
    assert(pair._2 == 5)
  }

  "read file" should "parse file" in {
    val rows = StoreCredit.parseFile("/small_file.txt")
    val firstRow = rows(0)
    assert(firstRow._1 == 100)
    assert(firstRow._2.length == 3)
  }

  "writeFile" should "write small text to file" in {
    val rows = StoreCredit.parseFile("/small_file.txt")
    val pairs = rows.map(r => StoreCredit.findPricePair(r._1, r._2))
    StoreCredit.writeFile("/Users/kpace/Documents/Code/small_file_solution.txt", pairs)
  }

  it should "write large text to file" in {
    val rows = StoreCredit.parseFile("/large_file.txt")
    val pairs = rows.map(r => StoreCredit.findPricePair(r._1, r._2))
    StoreCredit.writeFile("/Users/kpace/Documents/Code/large_file_solution.txt", pairs)
  }
}

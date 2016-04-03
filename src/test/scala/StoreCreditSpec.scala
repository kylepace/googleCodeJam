import org.scalatest._

class StoreCreditSpec extends FlatSpec with Matchers {
  def findPricePair(credit: Int, prices: List[Int]): (Int, Int) = {
    val pricesWithIndex = prices.zipWithIndex
    val pairs = for {
      first <- pricesWithIndex
      second <- pricesWithIndex if ((first._1 + second._1) == credit && (first._2 != second._2))
    } yield (first._2 + 1, second._2 + 1)

    pairs(0)
  }

  "findPricePair" should "return proper indexes in three item list" in {
    val credit = 4
    val pair = findPricePair(credit, List(1, 2, 3))
    assert(pair._1 == 1)
    assert(pair._2 == 3)
  }

  it should "return 2 3 with 100 credit" in {
    val credit = 100
    val pair = findPricePair(credit, List(5, 75, 25))
    assert(pair._1 == 2)
    assert(pair._2 == 3)
  }

  it should "return 1 4 with 200 credit" in {
    val credit = 200
    val pair = findPricePair(credit, List(150, 24, 79, 50, 88, 345, 3))
    assert(pair._1 == 1)
    assert(pair._2 == 4)
  }

  it should "return 4 5 with 8 credit" in {
    val credit = 8
    val pair = findPricePair(credit, List(2, 1, 9, 4, 4, 56, 90, 3))
    assert(pair._1 == 4)
    assert(pair._2 == 5)
  }
}

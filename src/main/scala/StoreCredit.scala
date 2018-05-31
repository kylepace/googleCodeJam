import java.io._

import scala.io.Source

/*
Problem

You receive a credit C at a local store and would like to buy two items. You first walk through the store and create a list L
of all available items. From this list you would like to buy two items that add up to the entire value of the credit. The
solution you provide will consist of the two integers indicating the positions of the items in your list (smaller number first).

Input

The first line of input gives the number of cases, N. N test cases follow. For each test case there will be:

    One line containing the value C, the amount of credit you have at the store.
    One line containing the value I, the number of items in the store.
    One line containing a space separated list of I integers. Each integer P indicates the price of an item in the store.
    Each test case will have exactly one solution.

Output

For each test case, output one line containing "Case #x: " followed by the indices of the two items whose price adds up to the store credit. The lower index should be output first.

Limits

5 ≤ C ≤ 1000
1 ≤ P ≤ 1000

Small dataset

N = 10
3 ≤ I ≤ 100

Large dataset

N = 50
3 ≤ I ≤ 2000
* */

object StoreCredit {
  def findPricePair(credit: Int, prices: List[Int]): (Int, Int) = {
    val pricesWithIndex = prices.zipWithIndex
    val pairs = for {
      first <- pricesWithIndex
      second <- pricesWithIndex if ((first._1 + second._1) == credit && (first._2 != second._2))
    } yield (first._2 + 1, second._2 + 1)

    pairs(0)
  }

  def parseFile(file: String): List[(Int, List[Int])] = {
    val sourceFile = Source.fromURL(getClass.getResource(file))

    val itemGroups = sourceFile.getLines().drop(1).grouped(3)

    itemGroups.map(ig => {
      (ig(0).toInt, ig(2).split(" ").map(c => c.toInt).toList)
    }).toList
  }

  def writeFile(file: String, pairs: List[(Int, Int)]) = {
    val writer = new PrintWriter(new File(file))
    val text = pairs.zipWithIndex.map(pair => s"Case #${pair._2 + 1}: ${pair._1._1} ${pair._1._2}\r\n").mkString
    writer.write(text)
    writer.close()
  }
}
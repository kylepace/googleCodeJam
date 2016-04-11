import java.io._

import scala.io.Source

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
import java.io.{File, PrintWriter}

import StoreCredit.getClass

import scala.io.Source

/*
Problem

Given a list of space separated words, reverse the order of the words. Each line of text contains L letters and W words.
A line will only consist of letters and space characters. There will be exactly one space character between each pair of consecutive words.

Input

The first line of input gives the number of cases, N.
N test cases follow. For each test case there will a line of letters and space characters indicating a list of space separated words.
Spaces will not appear at the start or end of a line.

Output

For each test case, output one line containing "Case #x: " followed by the list of words in reverse order.

Limits

Small dataset

N = 5
1 ≤ L ≤ 25

Large dataset

N = 100
1 ≤ L ≤ 1000

*/

object ReverseWords {
  def run(sentences: List[String]) = sentences map reverseWord

  def reverseWord(sentence: String) = sentence.split(" ").reverse.mkString(" ")

  def parseFile(file: String) =
    Source.fromURL(getClass.getResource(file))
      .getLines().drop(1).toList

  def writeFile(fileName: String, reversedSentences: List[String]) = {
    val writer = new PrintWriter(new File(fileName))
    val text = reversedSentences.zipWithIndex.map(pair => s"Case #${pair._2 + 1}: ${pair._1}\r\n").mkString
    writer.write(text)
    writer.close()
  }
}

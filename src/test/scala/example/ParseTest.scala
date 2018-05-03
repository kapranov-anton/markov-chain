package example

import org.scalatest.FunSuite

import scala.util.Random

class ParseTest extends FunSuite {
  val newsFile = "./WMT-News/raw/ru/newstest2012.xml.gz"
  val newsFileEn = "./WMT-News/raw/en/newstest2013.xml.gz"
  test("read gzip") {
    assert(Option(Parse.readZipped(newsFile)).nonEmpty)
  }

  test("read xml") {
    val firstLine = "Парламент не поддерживает поправку, дающую свободу Тимошенко"
    assert(Parse.fromXml(Parse.readZipped(newsFile)).head === firstLine)
  }

  test("tokenize") {
    val firstLine = "Парламент не поддерживает поправку, дающую свободу Тимошенко"
    val result = Parse.tokenize(List(firstLine))
    assert(result.head.w === "парламент")
    assert(result.size === 7)
  }

  test("word sequences") {
    val words = List("a", "b", "a", "c").map(Word(_))
    val result = Parse.wordSequences(words)
    assert(result === Map(Word("a") -> List(Word("b"), Word("c")), Word("b") -> List(Word("a"))))
  }

  test("markov") {
    val pairs = Parse.wordFromZip(newsFile)
    val chain = Parse.chain(pairs, Option(new Random(17)))
    val result = chain.map(_.w).mkString(" ")
    assert(result === "mss объявляет программу поиска гена ожирения.")
  }

  test("markov en") {
    val pairs = Parse.wordFromZip(newsFileEn)
    val chain = Parse.chain(pairs, Option(new Random(17)))
    val result = chain.map(_.w).mkString(" ")
    assert(result === "also attended the very largest party and democrats on wednesday.")
  }

  test("normalize word") {
    val word = Parse.normalizeWord("""asdasd"():""")
    assert(word === "asdasd")
  }

  test("xxx") {
    val pairs = Parse.wordFromZip(newsFile)
    val chain = Parse.chain(pairs)
    val result = chain.map(_.w).mkString(" ")
    println(result.capitalize)
  }

}

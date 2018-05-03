package example

import java.io.{BufferedInputStream, FileInputStream, InputStream}
import java.util.zip.GZIPInputStream

import scala.annotation.tailrec
import scala.collection.immutable
import scala.util.Random
import scala.xml.XML

case class Word(w: String, isFirst: Boolean = false)

object Parse {
  def readZipped(s: String): GZIPInputStream =
    new GZIPInputStream(new BufferedInputStream(new FileInputStream(s)))

  def fromXml(is: InputStream): immutable.Seq[String] = (XML.load(is) \\ "s").map(_.text)

  def isEndWord(s: String): Boolean = s.endsWith(".") || s.endsWith("?") || s.endsWith("!")

  def addFullStop(wordList: List[String]): List[String] =
    wordList.lastOption.map { s =>
      wordList.init :+ (if (isEndWord(s)) s else s ++ ".")
    }.getOrElse(List.empty[String])

  def normalizeWord(s: String): String = if (s == "-" ) "" else s.replaceAll("""["\():]+""", "").trim.toLowerCase

  def normalizeLine(l: String): List[String] = addFullStop(l.split(' ').map(normalizeWord).filter(_.nonEmpty).toList)

  def tokenize(lines: Seq[String]): Seq[Word] =
    for {
      l <- lines
      w <- normalizeLine(l) match {
        case Nil => Nil
        case h :: t => Word(h, isFirst = true) :: t.map(Word(_))
      }
    } yield w

  def groupByFirst[A, B](pairs: List[(A, B)]): Map[A, List[B]] =
    pairs.groupBy(_._1).mapValues(_.map(_._2))

  type WordSeq = Map[Word, List[Word]]

  def wordSequences(words: Seq[Word]): WordSeq =
    groupByFirst(words.sliding(2).map { case Seq(x, y) => (x, y) }.toList)

  val wordFromZip: String => WordSeq =
    readZipped _ andThen fromXml andThen tokenize andThen wordSequences

  def randElem[A](xs: List[A], random: Option[Random] = None): Option[A] = {
    xs match {
      case Nil => None
      case nel =>
        val rand = random.getOrElse(new Random(System.currentTimeMillis))
        val random_index = rand.nextInt(nel.length)
        Option(nel(random_index))
    }
  }

  @tailrec
  def chain(wordSeq: WordSeq, random: Option[Random] = None, acc: List[Word] = Nil, maxLen: Int = 50): List[Word] = {
    acc match {
      case Nil =>
        val keys = wordSeq.keys.filter(_.isFirst).toList
        Parse.randElem(keys, random) match {
          case Some(randKey) =>
            chain(wordSeq, random, List(randKey))
          case None => Nil
        }
      case words =>
        val lastWord = words.head
        if (isEndWord(lastWord.w) || words.size > maxLen) {
          words.reverse
        } else {
          val lastWords = acc.take(4).toSet
          val candidates = wordSeq(lastWord).filter(w => !lastWords.contains(w))
          Parse.randElem(candidates, random) match {
            case Some(randKey) =>
              chain(wordSeq, random, randKey :: words)
            case None => words.reverse
          }
        }
    }
  }
}
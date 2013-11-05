package yascc.input

import java.io.{ FileNotFoundException, IOException }

import scala.util.parsing.combinator.{ RegexParsers, PackratParsers }
import scala.util.parsing.input.{ Reader, StreamReader }
import scala.io.Source

import yascc.tree.Trees._
import yascc.util.Result

class FileParser extends RegexParsers
with PackratParsers
with ParserUtils
with GrammarParser
with TypeParser
with TreeParser
with SettingsParser
with DeclParser {
  import yascc.util.Implicits._

  /*

  Limitations:
  - backquote escape
  - opchar (e.g. '+', '-', etc. as valid identifiers)
  */

  val comment = "((/\\*([^*]|[\r\n]|(\\*+([^*/]|[\r\n])))*\\*+/)|(//.*))"

  // whiteSpace handles comments
  override protected val whiteSpace = s"($comment|([ \t\r\n]))+".r
  override def skipWhitespace = true

  private val sectionParsers: Map[String, Parser[Section]] = Map(
    ("grammar" -> grammar),
    ("tree" -> treeDefs),
    ("settings" -> settings),
    ("declarations" -> declarations)
  )

  private lazy val sectionName: PackratParser[String] = 
    "#[A-Za-z0-9]+".r ^^ ((x: String) => x.tail)

  private lazy val section: PackratParser[Section] = 
    Parser {
      in =>
        sectionName(in) match {
          case Success(name, in1) =>
            sectionParsers.get(name).map(_(in1)).getOrElse(Error(s"Illegal section name '$name'", in))
          case ns: NoSuccess => ns
        }
    }

  private lazy val file: PackratParser[File] = positioned(rep1(section) ^^ File.apply)

  private def parseFile(in: CharSequence): Result[File] = {
    //import yascc.util.Implicits._

    parseAll(file, in) match {
      case Success(f, _) => f.success
      case ns: NoSuccess => error(ns.msg)
    }
  }

  def readFile(fname: String): Result[File] = {
    try {
      parseFile(Source.fromFile(fname) mkString) // TODO: FIXME
    } catch {
      case fnf: FileNotFoundException =>
        error(fnf.toString)
      case ioe: IOException =>
        error(ioe.toString)
    }
  }
}

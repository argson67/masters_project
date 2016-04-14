package mypackage

import yascc.combinators._
import yascc.combinators.Defaults._

object SampleParser extends Parsers with SampleParserTrees with SampleParserTreesImpl {

  lazy val expr: Parser[Times] = rule("expr") {
    
    add ~ ("*" ~> add) ^^ { case x1 ~ x2 => Times.apply(x1, x2) }
  }
  
  lazy val program: Parser[Seq[Times]] = rule("program") {
    
    rep(expr) ^^ { x => (x) }
  }
  
  lazy val num: Parser[Num] = rule("num") {
    
    "[0-9]+".r ^^ { x => Num.apply(x) }
  }
  
  lazy val add: Parser[Plus] = rule("add") {
    
    num ~ ("+" ~> num) ^^ { case x1 ~ x2 => Plus.apply(x1, x2) }
  }
  
  override protected val whiteSpace = """[\n\t ]+""".r
  
  def apply(in: String) = {
    parseAll(program, in) match {
      case Left(e) => println(e); scala.sys.error("Syntax error.")
      case Right(v) => v
    }
  }
}
package imperative

import yascc.combinators._
import yascc.combinators.Defaults._

object SampleParser extends Parsers with SampleParserTrees with SampleParserTreesImpl with SampleParserUtils with SampleParserUtilsImpl {

  lazy val number: Parser[Num] = lrule("number") {
    
    "[0-9]+".r ^^ { x => strToInt.apply(x) }
  }
  
  lazy val expr: Parser[Expr] = rule("expression") {
    
    rep1sepc(recoverSkip(mulExpr, Seq(")", "}", ";", "+"))
      , "+"
      , Some(mulExpr)
      ) ^^ { x => leftAssocAdd.apply(x) }
  }
  
  lazy val statement: Parser[Stmt] = rule("statement") {
    
    whileStatement ^^ { x => (x) } |
    ifStatement ^^ { x => (x) } |
    exprStatement ^^ { x => (x) }
  }
  
  lazy val simpleExpr: Parser[Expr] = rule("simple expression") {
    
    number ^^ { x => (x) } |
    "(" ~!> (recoverSkip(expr, Seq(")", "}", ";")) <~ recoverInsert(")")) ^^ {
      x => (x)
    } |
    boolean ^^ { x => (x) }
  }
  
  lazy val whileStatement: Parser[WhileStmt] = lrule("whileStatement") {
    
    "while" ~!> (recoverInsert("(") ~> (recoverSkip(expr
      , Seq(")", "}", ";")
      ) ~ (recoverInsert(")") ~> (recoverInsert("{") ~> (recoverSkip(expr
      , Seq(")", "}", ";")
      ) <~ recoverInsert("}")))))) ^^ {
      case x1 ~ x2 => WhileStmt.apply(x1, x2)
    }
  }
  
  lazy val ifStatement: Parser[IfStmt] = lrule("ifStatement") {
    
    "if" ~!> (recoverInsert("(") ~> (recoverSkip(expr, Seq(")", "}", ";")) ~ (recoverInsert(")"
      ) ~> (recoverInsert("{") ~> (recoverSkip(expr, Seq(")", "}", ";")) ~ (recoverInsert("}"
      ) ~> opt("else" ~!> (recoverInsert("{") ~> (recoverSkip(expr
        , Seq(")", "}", ";")
        ) <~ recoverInsert("}")))
      ))))))) ^^ { case x1 ~ (x2 ~ x3) => IfStmt.apply(x1, x2, x3) }
  }
  
  lazy val exprStatement: Parser[ExprStmt] = rule("exprStatement") {
    
    expr ^^ { x => ExprStmt.apply(x) }
  }
  
  lazy val boolean: Parser[Boolean] = rule("boolean") {
    
    "true" ^^ { x => Boolean.apply(x) } |
    "false" ^^ { x => Boolean.apply(x) }
  }
  
  lazy val mulExpr: Parser[Expr] = rule("multiply expression") {
    
    rep1sepc(recoverSkip(simpleExpr, Seq("*", ";", "}", ")", "+"))
      , "*"
      , Some(simpleExpr)
      ) ^^ { x => leftAssocMultiply.apply(x) }
  }
  
  lazy val program: Parser[Program] = rule("program") {
    
    rep1sep(statement, ";") ^^ { x => Program.apply(x) }
  }
  
  override protected val whiteSpace = """[\n\t ]+""".r
  
  def apply(in: String) = {
    parseAll(program, in) match {
      case Left(e) => println(e); scala.sys.error("Syntax error.")
      case Right(v) => v
    }
  }
}
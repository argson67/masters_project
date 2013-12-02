package yascc.test.imperative

import scala.util.parsing.combinator.RegexParsers
import Trees._

object HandOldParser extends RegexParsers {
  override protected val whiteSpace = """[\n\t ]+""".r

  def apply(prgm: String) = 
    parseAll(program, prgm)

  lazy val program: Parser[Program] = 
    rep1sep(statement, ";") ^^ Program.apply

  lazy val statement: Parser[Stmt] = 
    whileStmt | ifStmt | exprStmt

  lazy val whileStmt: Parser[Stmt] = 
    "while" ~! ("(" ~> expr <~ ")") ~ ("{" ~> expr <~ "}") ^^ {
      case _ ~ cond ~ body => WhileStmt(cond, body)
    }

  lazy val ifStmt: Parser[Stmt] = 
    "if" ~! ("(" ~> expr <~")") ~ ("{" ~> expr <~ "}") ~ opt("else" ~> ("{" ~> expr <~ "}")) ^^ {
      case _ ~ cond ~ ifBranch ~ elseBranch => IfStmt(cond, ifBranch, elseBranch)
    }

  lazy val exprStmt: Parser[Stmt] =
    expr ^^ ExprStmt.apply

  lazy val expr: Parser[Expr] =
    rep1sep(mulExpr, "+") ^^ leftAssocAdd

  lazy val mulExpr: Parser[Expr] =
    rep1sep(simpleExpr, "*") ^^ leftAssocMultiply

  lazy val simpleExpr: Parser[Expr] =
    num | ("(" ~> expr <~ ")")

  lazy val num: Parser[Expr] =
    "[0-9]+".r ^^ { n => Num(Integer.parseInt(n)) }
    
  // utils

  def leftAssoc(es: Seq[Expr], op: (Expr, Expr) => Expr) = 
    es reduceLeft op

  def leftAssocAdd(es: Seq[Expr]) = 
    leftAssoc(es, Add.apply)

  def leftAssocMultiply(es: Seq[Expr]) =
    leftAssoc(es, Multiply.apply)
}

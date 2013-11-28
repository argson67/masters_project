package yascc.test.imperative

import yascc.combinators._
import Trees._

object HandParser extends Parsers {
  override protected val whiteSpace = """[\n\t ]+""".r

  def apply(prgm: String) = 
    parseAll(program, prgm)

  lazy val program: Parser[Program] = rule("program") {
    rep1sep(statement, ";") ^^ Program.apply
  }

  lazy val statement: Parser[Stmt] = rule("statement") {
    whileStmt | ifStmt | exprStmt
  }

  lazy val whileStmt: Parser[Stmt] = lrule("while statement") {
    "while" ~!> (recoverInsert("(") ~> expr <~ recoverInsert(")")) ~ (recoverInsert("{") ~> expr <~ recoverInsert("}")) ^^ {
      case cond ~ body => WhileStmt(cond, body)
    }
  }

  lazy val ifStmt: Parser[Stmt] = lrule("if statement") {
    "if" ~!> (recoverInsert("(") ~> expr <~ recoverInsert(")")) ~
      (recoverInsert("{") ~> expr <~ recoverInsert("}")) ~
      opt("else" ~> (recoverInsert("{") ~> expr <~ recoverInsert("}"))) ^^ {
        case cond ~ ifBranch ~ elseBranch => IfStmt(cond, ifBranch, elseBranch)
      }
  }

  lazy val exprStmt: Parser[Stmt] = rule("expression statement") {
    expr ^^ ExprStmt.apply
  }

  lazy val expr: Parser[Expr] = lrule("expression") {
    rep1sepc(mulExpr, "+") ^^ leftAssocAdd
  }

  lazy val mulExpr: Parser[Expr] = rule("multiply expression") {
    rep1sepc(simpleExpr, "*") ^^ leftAssocMultiply
  }

  lazy val simpleExpr: Parser[Expr] = rule("simple expression") {
    num | ("(" ~> expr <~ ")")
  }

  lazy val num: Parser[Expr] = lrule("number") {
    "[0-9]+".r ^^ { n => Num(Integer.parseInt(n)) }
  }

  // utils

  def leftAssoc(es: Seq[Expr], op: (Expr, Expr) => Expr) = 
    es reduceLeft op

  def leftAssocAdd(es: Seq[Expr]) = 
    leftAssoc(es, Add.apply)

  def leftAssocMultiply(es: Seq[Expr]) =
    leftAssoc(es, Multiply.apply)
}
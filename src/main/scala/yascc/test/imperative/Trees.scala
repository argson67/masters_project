package  yascc.test.imperative

object Trees {
  case class Program(stmts: Seq[Stmt])

  trait Expr

  case class Num(n: Int) extends Expr
  case class Add(op1: Expr, op2: Expr) extends Expr
  case class Multiply(op1: Expr, op2: Expr) extends Expr

  trait Stmt

  case class WhileStmt(cond: Expr, body: Expr) extends Stmt
  case class IfStmt(cond: Expr, ifBranch: Expr, elseBranch: Option[Expr]) extends Stmt
  case class ExprStmt(e: Expr) extends Stmt
}
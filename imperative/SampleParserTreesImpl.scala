package imperative

import scala.util.parsing.input.Positional

trait SampleParserTreesImpl extends SampleParserTrees {
  case class WhileStmt(cond: Expr, body: Expr) extends Stmt with Positional {
  
    
  }
  
  case class IfStmt(cond: Expr, ifBranch: Expr, elseBranch: Option[Expr]) extends Stmt with Positional {
  
    
  }
  
  trait Expr extends Term with Positional {  }
  
  case class Num(n: Int) extends Expr with Positional {  }
  
  case class ExprStmt(e: Expr) extends Stmt with Positional {  }
  
  case class Program(stmts: Seq[Stmt]) extends Term with Positional {  }
  
  case class Times(op1: Expr, op2: Expr) extends Expr with Positional {  }
  
  case class Boolean(repr: String) extends Expr with Positional {  }
  
  case class Add(op1: Expr, op2: Expr) extends Expr with Positional {  }
  
  trait Stmt extends Term with Positional {  }
  
  trait Term extends Positional {  }
}
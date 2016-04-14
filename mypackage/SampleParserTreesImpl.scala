package mypackage

import scala.util.parsing.input.Positional

trait SampleParserTreesImpl extends SampleParserTrees {
  trait Expr extends Positional {  }
  
  case class Num(repr: String) extends Expr with Positional {  }
  
  case class Times(op1: Expr, op2: Expr) extends Expr with Positional {  }
  
  case class Plus(op1: Expr, op2: Expr) extends Expr with Positional {  }
}
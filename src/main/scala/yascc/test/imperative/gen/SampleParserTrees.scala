package imperative

import scala.util.parsing.input.Positional

trait SampleParserTrees {
  type WhileStmt <: Stmt with Positional
  
  type IfStmt <: Stmt with Positional
  
  type Expr <: Term with Positional
  
  type Num <: Expr with Positional
  
  type ExprStmt <: Stmt with Positional
  
  type Program <: Term with Positional
  
  type Times <: Expr with Positional
  
  type Boolean <: Expr with Positional
  
  type Add <: Expr with Positional
  
  type Stmt <: Term with Positional
  
  type Term <: Positional
}
package mypackage

import scala.util.parsing.input.Positional

trait SampleParserTrees {
  type Expr <: Positional
  
  type Num <: Expr with Positional
  
  type Times <: Expr with Positional
  
  type Plus <: Expr with Positional
}
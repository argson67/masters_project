package yascc.input

import scala.util.parsing.combinator.{ RegexParsers, PackratParsers }

import yascc.tree.Trees._

trait TreeParser {
  self: FileParser =>

    // entry point
    lazy val treeDefs: PackratParser[TreeDefs] = rep1(treeDef) ^^ TreeDefs.apply

    private[TreeParser] lazy val treeDef: PackratParser[TreeDef] = 
      treeBranch | treeLeaf

    private[TreeParser] lazy val treeBranch: PackratParser[TreeBranch] = positioned(
      identifier ~ braces(rep(treeDef)) ^^ {
        case name ~ children => TreeBranch(name, children)
      })

    private[TreeParser] lazy val treeLeaf: PackratParser[TreeLeaf] = positioned(
      identifier ~ parens(rep1sep(leafParam, ",")) ^^ {
        case name ~ params => TreeLeaf(name, params)
      })

    private[TreeParser] lazy val leafParam: PackratParser[(String, ParamType)] =
      identifier ~ (":" ~> paramType) ^^ {
        case name ~ tpe => (name, tpe)
      }
}

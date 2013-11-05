package yascc.input

import yascc.tree.Trees._

trait DeclParser {
  self: FileParser =>

    lazy val declarations: PackratParser[Declarations] = rep1(declaration) ^^ Declarations.apply

    private[DeclParser] lazy val declaration: PackratParser[Declaration] = positioned(
      identifier ~ (":" ~> scalaType) ^^ {
        case name ~ tpe => Declaration(name, tpe)
      }
      | identifier ~ parens(repsep(scalaType, ",")) ~ (":" ~> scalaType) ^^ {
        case name ~ paramTypes ~ retType => Declaration(name, FunctionType(paramTypes, retType))
      })
}

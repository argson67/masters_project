package yascc.input

import yascc.tree.Trees._

trait DeclParser {
  self: FileParser =>

    lazy val declarations: PackratParser[Declarations] = log(rep1(declaration) ^^ Declarations.apply)("declarationS")

    private[DeclParser] lazy val declaration: PackratParser[Declaration] = log(positioned(
      identifier ~ (":" ~> scalaType) ^^ {
        case name ~ tpe => Declaration(name, tpe)
      }
      | identifier ~ parens(repsep(paramType, ",")) ~ (":" ~> scalaType) ^^ {
        case name ~ paramTypes ~ retType => Declaration(name, FunctionType(paramTypes, retType))
      }))("declaration")
}

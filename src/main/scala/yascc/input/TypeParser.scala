package yascc.input

import yascc.tree.Trees._

trait TypeParser {
  self: FileParser =>

    private[TypeParser] type TypeMorph = ScalaType => ScalaType
    /*

    Limitations: (TODO?)
    - Compound types
    - Refinements
    - Annotations
    - ".type"
    - ".super"

    */

    private[TypeParser] lazy val idName: PackratParser[Name] = identifier ^^ Name

    // TODO: maybe rewrite with chainl1? Although, what's the point,
    // the cast is ugly enough as it is...
    private[TypeParser] lazy val stableId: PackratParser[Identifier] = 
      idName ~ repsep(idName, ".") ^^ {
        case id ~ ids => ((id: Identifier) /: ids)(Path.apply)
      }

    // Entry point
    lazy val scalaType: PackratParser[ScalaType] = functionType | infixType

    // Entry point
    lazy val functionType: PackratParser[ScalaType] = positioned(
      functionArgTypes ~ ("=>" ~> scalaType) ^^ {
        case args ~ ret => FunctionType(args.toList, ret)
      }
    )

    private[TypeParser] lazy val functionArgTypes: PackratParser[Seq[ParamType]] = parens(repsep(paramType, ","))

    // Entry point
    lazy val paramType: PackratParser[ParamType] = positioned(
      opt("=>") ~ scalaType ~ opt("*") ^^ {
        case isLazy ~ tpe ~ isRep => ParamType(tpe, isLazy.isDefined, isRep.isDefined)
      })

    private[TypeParser] lazy val infixType: PackratParser[ScalaType] = simpleType ~ typeMods ^^ {
      case tpe ~ mods => mods(tpe)
    }

    private[TypeParser] lazy val typeMods: PackratParser[TypeMorph] =
      rep(brackets(typeArgs) | typeProjection) ^^ (_.reduceOption(_ andThen _).getOrElse((x: ScalaType) => x))

    private def makeTypeApp(constructor: ScalaType, args: Seq[ScalaType]) = (constructor, args) match {
      case (OptionTypeConstructor, Seq(arg)) => OptionType(arg)
      case (SeqTypeConstructor(name), Seq(arg)) => SeqType(name, arg)
      case _ => TypeApp(constructor, args.toList)
    }

    private def readBasicType(id: Identifier): ScalaType = id match {
      case Name("Option") => OptionTypeConstructor
      case Name("Seq") => SeqTypeConstructor("Seq")
      case Name("List") => SeqTypeConstructor("List")
      case _ => SimpleType(id)
    }

    private[TypeParser] lazy val typeArgs: PackratParser[TypeMorph] = 
      rep1sep(scalaType, ",") ^^ (tpes => makeTypeApp(_: ScalaType, tpes))

    private[TypeParser] lazy val typeProjection: PackratParser[TypeMorph] =
      "#" ~> identifier ^^ (id => TypeProjection(_: ScalaType, id))

    private[TypeParser] lazy val simpleType: PackratParser[ScalaType] = positioned(
      parens(rep1sep(scalaType, ",")) ^^ TupleType.apply
      | stableId ^^ readBasicType)
}

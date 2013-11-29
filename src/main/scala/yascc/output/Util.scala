package yascc.output

import org.kiama.output.PrettyPrinter

import yascc.tree.Trees._

trait Utils extends PrettyPrinter {
  override val defaultIndent = 2

  implicit class ExtDoc(d: Doc) {
    def <@@@>(e: Doc) =
      d <> linebreak <> linebreak <> e
  }

  def nest1(d: Doc): Doc = nest(empty <@> d)
  def braces1(d: Doc): Doc = group("{" <\> nest1(d) <@> "}")
  def parens1(d: Doc, b: Boolean = true): Doc = if (b) "(" <> d <> ")" else d
  def brackets1(d: Doc): Doc = group("[" <> d <> "]")

  def printType(tpe: ScalaType): Doc = tpe match {
    case UnTyped => "???"
    case FunctionType(args, returnType) => 
      parens(lsep(args map printType, comma)) <+> "=>" <+> printType(returnType)
    case ParamType(tpe, isLazy, isRep) =>
      printType(tpe)
    case TupleType(elems) =>
      parens(lsep(elems map printType, comma))
    case SimpleType(name) =>
      name.canonicalName
    case TypeApp(constructor, args) =>
      printType(constructor) <> brackets1(lsep(args map printType, comma))
    case TypeProjection(tpe, name) =>
      printType(tpe) <> "#" <> name
    case TVar(name) =>
      name

    // Internal types

    case Trait(name, parentOpt) =>
      name
    case CaseClass(name, params, parentOpt) =>
      name
    case UnknownType(name) =>
      name.canonicalName
    case ErrorType =>
      "*Error*"
    case AnyType =>
      "Any"
    case NothingType =>
      "Nothing"

    case SeqTypeConstructor(name) =>
      name
    case OptionTypeConstructor =>
      "Option"
    case OptionType(param) =>
      "Option" <> brackets1(printType(param))
    case SeqType(name, param) =>
      name <>  brackets1(printType(param))
  }
}

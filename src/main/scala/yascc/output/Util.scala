package yascc.output

import org.kiama.output.PrettyPrinter

import yascc.tree.Trees._

trait Utils extends PrettyPrinter {    
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
      printType(constructor) <> brackets(lsep(args map printType, comma))
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
      "Option" <> brackets(printType(param))
    case SeqType(name, param) =>
      name <>  brackets(printType(param))
  }
}

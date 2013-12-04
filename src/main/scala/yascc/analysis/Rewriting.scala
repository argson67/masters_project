package yascc.analysis

import yascc.tree.Trees._
import yascc.util.Result

trait Rewriting {
  self: Phases =>

  import yascc.util.Implicits._

  object RewriteTypesPhase extends Phase {
    private def rewriteParamType(p: ParamType): ParamType = 
      ParamType(rewriteType(p.tpe), p.isLazy, p.isRep)

    private def rewriteType(t: ScalaType): ScalaType = t match {
      // Types
      case UnTyped => UnTyped
      case FunctionType(args, returnType) =>
        FunctionType(args map rewriteParamType, rewriteType(returnType))
      case pt@ParamType(tpe, isLazy, isRep) =>
        rewriteParamType(pt)
      case TupleType(elems) =>
        TupleType(elems map rewriteType)
      case SimpleType(name) =>
        lookupType(name) map {
          case TypeSymbol(_, UnTyped) => UnknownType(name)
          case TypeSymbol(_, other) => other
        } get

      case SeqTypeConstructor(name) => SeqTypeConstructor(name)
      case OptionTypeConstructor => OptionTypeConstructor
      case OptionType(param) => OptionType(rewriteType(param))
      case SeqType(name, param) => SeqType(name, rewriteType(param))

      case TypeApp(constructor, args) =>
        TypeApp(rewriteType(constructor), args map rewriteType)
      case TypeProjection(tpe, name) =>
        TypeProjection(rewriteType(tpe), name)
      case TVar(name) => TVar(name)

      case other => other

    /*  // Internal types

      case Trait(name, parentOpt) =>
      case CaseClass(name, params, parentOpt) =>
      case UnknownType(name) =>
      case ErrorType =>
      case AnyType =>
      case NothingType => */
    }

    def apply(in: Result[Tree]): Result[Tree] = {
      val res = _allDecls map {
        ts =>
          setType(ts.name, rewriteType(ts.tpe))
      } toSeq

      in :+ Result.sequence(res)
    }
  }

  object RewritePhase extends Phase {

    // TODO: fix moar things?

    private def extractDiscard(pe: ProductionElem): ProductionElem = pe match {
      case Discard(Commit(x)) => Commit(Discard(x))
      case other => other
    }

    private def flattenConjunction(e: List[ProductionElem]): List[ProductionElem] = {
      e match {
        case Nil => Nil
        case Discard(Conjunction(es)) :: rest => 
          (es map (Discard.apply(_)).andThen(extractDiscard)) ++ flattenConjunction(rest)
        case x :: xs => x :: flattenConjunction(xs)
      }
    }

    private def fixTree(t: Tree): Result[Tree] = rewrite(t) {
      case Commit(o@ Opt(_)) => o.failure("Cannot commit after an option", o.pos)
      case Rep(Commit(r), s, strict) => Rep(r, s, strict).failure("Commit inside a rep(sep)", r.pos)
      //case Rep(r, Some(Commit(s)), strict) => Rep(r, Some(s), strict).failure("Commit inside a repsep", s.pos)
      case Label(Commit(t), name) => Commit(Label(t, name))
      case Commit(r: Rep) => r.failure("Rep inside a commit. Don't do it.", r.pos)

      case Rep(Discard(x), s, strict) => Rep(x, s, strict).failure("Discard inside a rep. Don't do it.", t.pos)

      case Conjunction(List(e)) => e
      case Conjunction(elems) => Conjunction(flattenConjunction(elems))

      case Discard(Commit(x)) => Commit(Discard(x))
    }

    def apply(in: Result[Tree]): Result[Tree] = {
      in flatMap fixTree
    }
  }
}
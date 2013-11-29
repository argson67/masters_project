package yascc.analysis

import scala.language.implicitConversions

import yascc.tree.Trees._
import yascc.util.Result

import yascc.util.Substitution

trait Typechecker {
  self: Phases =>

    object TyperPhase extends Phase {
      import yascc.util.Implicits._

      private implicit def _lookupType(name: Identifier): ScalaType = 
        lookupType(name).get.tpe
      private var _tvarCounter = 0;
      private def newTypeVar = {
        val result = s"Θ${_tvarCounter}"
        _tvarCounter += 1
        TVar(result)
      }

      private def varAsgn(v: TVar, t: ScalaType): Result[(ScalaType, Substitution)] = {
        if (v == t) {
          (t, Substitution.empty).success
        } else if (t.freeVars contains v) {
          (ErrorType, Substitution.empty).failure(s"occurs check fails: $v in $t")
        } else {
          (t, Substitution(v -> t)).success
        }
      }

      private def checkArg(expected: ScalaType, given: ScalaType, name: String, i: Int): Result[Substitution] = {
        (given :< expected) flatMap {
          if (_) {
            Substitution.empty
          } else {
            (expected, given) match {
              case (v: TVar, t: ScalaType) =>
                varAsgn(v, t) map (_._2)
              case (t: ScalaType, v: TVar) =>
                varAsgn(v, t) map (_._2)
              case _ =>
                Substitution.empty.failure(s"Illegal argument to $name in position $i. Expected $expected, given $given.")
            }
          }
        }
      }

      private def unify(t1: ScalaType, t2: ScalaType): Result[(ScalaType, Substitution)] = {
        (t1 :< t2) flatMap { c1 => if (c1) {
          (t2, Substitution.empty).success
        } else { (t1 :> t2) flatMap { c2 => if (c2) {
          (t1, Substitution.empty).success
        } else {
          (t1, t2) match {
            case (v: TVar, t: ScalaType) =>
              varAsgn(v, t)
            case (t: ScalaType, v: TVar) =>
              varAsgn(v, t)
            case (UserType(_, Some(p1)), UserType(_, Some(p2))) =>
              unify(p1, p2)
            case _ =>
              (ErrorType, Substitution.empty).failure(s"Cannot unify $t1 and $t2")
          }
        } } } }
      }

      private def typeElem(e: ProductionElem): Result[(ScalaType, Substitution)] = e match {
        // Production elements
        case Conjunction(elems) =>
          Result.sequence(elems filter (!_.isDiscarded) map typeElem) map {
            tss => 
              val (ts, subs) = tss.unzip
              val tpe = if (ts.size == 0) {
                UnknownType(Name("Unit"))
              } else if (ts.size == 1) {
                ts.head
              } else {
                TupleType(ts.toList)
              }
              (tpe, subs reduce (_ + _))
          }
        case Disjunction(elems) =>
          Result.sequence(elems map typeElem) flatMap superType
        case Opt(elem) =>
          typeElem(elem) map {
            case (t, sub) =>
              (OptionType(t), sub)
          }
        case Commit(elem) =>
          typeElem(elem)
        case Discard(elem) =>
          typeElem(elem) +: (UnknownType(Name("Unit")), Substitution.empty).success
        case Rep(elem, sep, strict) =>
          typeElem(elem) map {
            case (t, sub) =>
              (SeqType("Seq", t), sub)
          }
        case Label(elem, label) =>
          typeElem(e)
        case NonTerminal(name) =>
          getType(name, e.pos) map {
            ts => (ts, Substitution.empty)
          }

        // Terminals
        case NumberLiteral(num) =>
          (UnknownType(Name("Int")), Substitution.empty).success // TODO: FIXME
        case StringLiteral(str) =>
          (UnknownType(Name("String")), Substitution.empty).success
        case RegexLiteral(regex) =>
          (UnknownType(Name("String")), Substitution.empty).success
        case CharLiteral(c) =>
          (UnknownType(Name("Char")), Substitution.empty).success
        case Epsilon =>
          (ErrorType, Substitution.empty).failure("Epsilon in production???")
      }

      private def typeProduction(p: Production): Result[(ScalaType, Substitution)] = {
        val body = p.body
        val action = p.action

        typeElem(body) flatMap {
          case bodyTS@(tpe, sub) => 
            val arity = tpe match {
              case TupleType(ts) => ts.length
              case _ => 1
            }

            p.arity = arity
            
            action match {
              case FunctionAction(fun) =>
                val argTypes = tpe match {
                  case TupleType(ts) => ts
                  case other => Seq(other)
                }
                lookupFunction(fun).flatMap(f => typeApplication(fun.canonicalName, f, argTypes))
              case CustomAction(code, t) =>
                (t, sub).success
              case DefaultAction =>
                bodyTS.success
            }
        }
      }

      private def lookupFunction(name: Identifier): Result[FunctionType] = {
        lookupValue(name.canonicalName, name.pos) flatMap {
          _.tpe match {
            case ft: FunctionType => ft.success
            case other => error(s"Illegal action '${name.canonicalName}': not a function type: ${other}", name.pos)
          }
        }
      }

      private def checkArgs(formals: Seq[ParamType], actuals: Seq[ScalaType], name: String, i: Int = 1): Result[Substitution] = {
        if (formals.size == 0) {
          if (actuals.size > 0) {
            Substitution.empty.failure(s"Too many arguments to function $name")
          } else {
            Substitution.empty.success
          }
        } else if (actuals.size == 0) {
          if (formals.head.isRep) {
            Substitution.empty.success
          } else {
            Substitution.empty.failure("Too few arguments to function $name")
          }
        } else {
          checkArg(formals.head.tpe, actuals.head, name, i) flatMap {
            sub =>
              val newFormals = if (formals.head.isRep) formals else formals.tail
              checkArgs(newFormals, actuals.tail, name, i+1) map (s => sub + s) 
          }
        }
      }

      private def typeApplication(name: String, f: FunctionType, args: Seq[ScalaType]): Result[(ScalaType, Substitution)] = {
        println(s"Calling typeApplication($name, $f, $args)")
        checkArgs(f.args, args, name) map {
          sub => (f.returnType.substitute(sub), sub)
        }
      }

      private def superType(seq: Seq[(ScalaType, Substitution)]): Result[(ScalaType, Substitution)] = {
        if (seq.isEmpty) {
          (ErrorType, Substitution.empty).failure("Cannot determine the supertype of an empty sequence")
        } else if (seq.length == 1) {
          return seq.head.success
        } else {
          val (headType, headSub) = seq.head
          superType(seq.tail) flatMap {
            case (tailType, tailSub) =>
              val sub = headSub + tailSub
              unify(headType.substitute(tailSub), tailType.substitute(headSub)) map {
                case (t, s) =>
                  (t, sub + s)
              }
          }
        }
      }

      private def typeRule(r: Rule): Result[(ScalaType, Substitution)] = {
        Result.sequence(r.productions map {
          p => typeProduction(p)
        }) flatMap (seq => superType(seq))
      }

      private def typeRules(rs: Set[String]) = {
        val rulesVars = rs map (name => (lookupRule(name).get.r, newTypeVar))

        for ((r, v) <- rulesVars) {
          setType(r.name, v)
        }

        val subs = rulesVars map {
          case (r, v) =>
            for ( (tpe, s1) <- typeRule(r)
                ; (_, s2) <- varAsgn(v, tpe.substitute(s1))) 
            yield s1 + s2
        }

        val sub = Result.sequence(subs.toSeq) map (s => s.reduceLeft(_ + _))
        sub foreach {
          s =>
            rulesVars foreach {
              case (r, v) =>
                setType(r.name, v.substitute(s))
            }
        }

        sub.toUnit
      }

      def apply(in: Result[Tree]): Result[Tree] = {
        val typingOrder = getTypingOrder(rules.toSeq)
        println(s"Typing order: $typingOrder")
        Result.sequence(typingOrder map typeRules) +: in
      }
    }
}

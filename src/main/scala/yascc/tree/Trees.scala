package yascc.tree

import scala.util.parsing.input.Positional
//import scala.collection.mutable.{ Set => MSet }

import org.kiama.attribution.Attributable

import yascc.util.{ Substitution, Substitutable }

/*
Pattern matching on trees

case File(sections) =>

// Identifiers
case Path(prefix, name) =>
case Name(name) =>

// Grammar
case Grammar(rules) =>

case Rule(name, options, productions) =>
case RuleOption(name, value) =>
case Production(body, action) =>

// Production elements
case Conjunction(elems) =>
case Disjunction(elems) =>
case Opt(elem) =>
case Commit(elem) =>
case Discard(elem) =>
case Rep(elem, sep, strict) =>
case Label(elem, label) =>
case NonTerminal(name) =>

// Terminals
case NumberLiteral(num) =>
case StringLiteral(str) =>
case RegexLiteral(regex) =>
case CharLiteral(c) =>
case Epsilon =>

// Actions
case FunctionAction(fun) =>
case CustomAction(code, tpe) =>
case DefaultAction =>

// Types
case UnTyped =>
case FunctionType(args, returnType) =>
case ParamType(tpe, isLazy, isRep) =>
case TupleType(elems) =>
case SimpleType(name) =>
case TypeApp(constructor, args) =>
case TypeProjection(tpe, name) =>
case TVar(name) =>

// Internal types

case Trait(name, parentOpt) =>
case CaseClass(name, params, parentOpt) =>
case UnknownType(name) =>
case ErrorType =>
case AnyType =>
case NothingType =>

case SeqTypeConstructor(name) =>
case OptionTypeConstructor =>
case OptionType(param) =>
case SeqType(name, param) =>

// Tree defs
case TreeDefs(defs) =>
case TreeBranch(name, myChildren) =>
case TreeLeaf(name, params) =>

// Settings
case Settings(defs) =>
case Setting(name, value) =>

// Declarations
case Declarations(decls) =>
case Declaration(name, tpe) =>

*/

/*
// Sections
case Settings(defs) =>
case Declarations(decls) =>
case TreeDefs(defs) =>
case Grammar(rules) =>
*/

object Trees {
  sealed abstract class Tree extends Product with Positional with Attributable {

  }

  sealed abstract class Identifier() extends Tree {
    def canonicalName: String
  }

  case class Path(prefix: Identifier, name: Name) extends Identifier {
    val canonicalName = s"${prefix.canonicalName}.${name.canonicalName}"
  }

  case class Name(name: String) extends Identifier {
    val canonicalName = name
  }

  // TODO: Maybe separate these into... separate files? 

  // File stuff

  case class File(sections: List[Section]) extends Tree {

  }

  sealed abstract class Section(name: String) extends Tree {

  }

  // Grammar stuff

  case class Grammar(rules: List[Rule]) extends Section("grammar") {

  }

  case class Rule(name: String, options: List[RuleOption], productions: List[Production]) extends Tree {
    var refCount: Int = 0
    var firstSet: Set[Terminal] = Set.empty
    var followSet: Set[Terminal] = Set.empty

    private lazy val optionsMap = options map { ro => (ro.name, ro.value) } toMap

    def hasOption(n: String) = optionsMap contains n
    def option(n: String) = optionsMap.get(n)
  }

  val ErrorRule = Rule("ERROR", List.empty, List.empty)

  case class RuleOption(name: String, value: Option[String]) extends Tree {

  }

  case class Production(body: ProductionElem, action: Action) extends Tree {
    var arity: Int = -1
  }

  /* Production elements */

  sealed abstract class ProductionElem extends Tree {
    def isDiscarded: Boolean = false
  }

  case class Conjunction(elems: List[ProductionElem]) extends ProductionElem {
    override val isDiscarded = elems.size == 1 && elems.head.isDiscarded
  }

  case class Disjunction(elems: List[ProductionElem]) extends ProductionElem {
    override val isDiscarded = elems.size == 1 && elems.head.isDiscarded
  }

  case class Opt(elem: ProductionElem) extends ProductionElem {
    override val isDiscarded = elem.isDiscarded
  }

  case class Commit(elem: ProductionElem) extends ProductionElem {
    override val isDiscarded = elem.isDiscarded
  }

  case class Discard(elem: ProductionElem) extends ProductionElem {
    override val isDiscarded = true
  }

  case class Rep(elem: ProductionElem, sep: Option[ProductionElem] = None, strict: Boolean = false) extends ProductionElem {
    override val isDiscarded = elem.isDiscarded
  }

  case class Label(elem: ProductionElem, label: String) extends ProductionElem {
    override val isDiscarded = elem.isDiscarded
  }

  case class NonTerminal(name: String) extends ProductionElem {

  }

  sealed abstract class Terminal extends ProductionElem {
    
  }

  case class NumberLiteral[T : Numeric](num: T) extends Terminal {

  }

  case class StringLiteral(str: String) extends Terminal {

  }

  case class RegexLiteral(regex: String) extends Terminal {

  }

  // Do we need this??
  case class CharLiteral(c: Char) extends Terminal {

  }

  case object Epsilon extends Terminal {
    override def toString = "\u03B5" // :-)
  }

  /* Actions */

  sealed abstract class Action extends Tree {

  }

  case class FunctionAction(fun: Identifier) extends Action {

  }

  case class CustomAction(code: String, tpe: ScalaType) extends Action {

  }

  case object DefaultAction extends Action {

  }

  /* Types */

  // TODO

  import yascc.util.Result
  import yascc.util.Implicits._

  
  implicit class BoolResOps(b: Boolean) {
    def ||(rb: => Result[Boolean]): Result[Boolean] =
      if (b) true.success else rb
    def &&(rb: => Result[Boolean]): Result[Boolean] =
      if (!b) false.success else rb
  }

  implicit class ResBoolOps(rb: Result[Boolean]) {
    def ||(rb1: => Result[Boolean]): Result[Boolean] = 
      rb flatMap { b => if (b) true.success else rb1 }
    //def ||(b1: => Boolean): Result[Boolean] = 
    //  rb map { b => b || b1 }

    def &&(rb1: => Result[Boolean]): Result[Boolean] = 
      rb flatMap { b => if (!b) false.success else rb1 }
    //def &&(b1: => Boolean): Result[Boolean] = 
    //  rb map { b => b && b1 }
  }

  sealed abstract class ScalaType() extends Tree with Substitutable[ScalaType] {
    def :< (other: ScalaType)(implicit lookupType: Identifier => ScalaType): Result[Boolean] = {

      //println(s"*** CALLING $this :< $other")

      val rOther = other match {
        case SimpleType(n) => lookupType(n)
        case other => other
      }

      val res = (this == rOther) || (rOther == AnyType) || _isSubtypeOf(rOther)
      if (this.isInstanceOf[UnknownType]) {
        res.addWarning(s"Cannot check whether $this conforms to $other, $this is an unknown type.")
      } else if (rOther.isInstanceOf[UnknownType]) {
        res.addWarning(s"Cannot check whether $this conforms to $other, $other is an unknown type.")
      } else {
        res
      }
    }

    def :> (other: ScalaType)(implicit lookupType: Identifier => ScalaType): Result[Boolean] = 
      other :< this

    protected def _isSubtypeOf(other: ScalaType)(implicit lookupType: Identifier => ScalaType): Result[Boolean] = 
      false.success

    def substitute(sub: Substitution) = this

    lazy val freeVars: Set[TVar] = Set.empty

    override def toString = TreePrinter(this)

    val isPositional = false
  }

  case object UnTyped extends ScalaType {

  }

  case class FunctionType(args: List[ParamType], returnType: ScalaType) extends ScalaType {
    override protected def _isSubtypeOf (other: ScalaType)(implicit lookupType: Identifier => ScalaType) = other match {
      case FunctionType(otherArgs, otherReturnType) =>
        Result.sequence((args zip otherArgs) map { case (t1, t2) => t2 :< t1 }) map (_.forall(identity))

        (args zip otherArgs map { case (t1, t2) => t2 :< t1 } reduce (_ && _)) && returnType :< otherReturnType
      case _ => false.success
    }

    override def substitute(sub: Substitution) = 
      FunctionType(args map (_ substitute sub), returnType.substitute(sub)) setPos pos

    override lazy val freeVars = (args flatMap (_.freeVars) toSet) ++ returnType.freeVars
  }

  case class ParamType(tpe: ScalaType, isLazy: Boolean = true, isRep: Boolean = true) extends ScalaType {
    override protected def _isSubtypeOf (other: ScalaType)(implicit lookupType: Identifier => ScalaType) = other match {
      case ParamType(otherTpe, _, otherIsRep) if isRep == otherIsRep =>
        tpe :< otherTpe
      case _ => false
    }

    override def substitute(sub: Substitution) = 
      ParamType(tpe.substitute(sub), isLazy, isRep) setPos pos

    override lazy val freeVars = tpe.freeVars
  }

  case class TupleType(elems: List[ScalaType]) extends ScalaType {
    override protected def _isSubtypeOf (other: ScalaType)(implicit lookupType: Identifier => ScalaType) = other match {
      case TupleType(otherElems) => 
        (elems.size == otherElems.size).success && (elems zip otherElems map { case (t1, t2) => t1 :< t2 } reduce (_ && _))
      case _ => false
    }

    override def substitute(sub: Substitution) =
      TupleType(elems map (_ substitute sub)) setPos pos

    override lazy val freeVars = elems flatMap (_.freeVars) toSet
  }

  case class SimpleType(name: Identifier) extends ScalaType {
    override def :< (other: ScalaType)(implicit lookupType: Identifier => ScalaType): Result[Boolean] = 
      lookupType(name) :< other
  }

  case object AnyType extends ScalaType {

  }

  case object NothingType extends ScalaType {
    // Nothing is a subtype of everything
    override protected def _isSubtypeOf(other: ScalaType)(implicit lookupType: Identifier => ScalaType) = 
      true
  }

  sealed abstract class Variance
  case object InVariant extends Variance
  case object CoVariant extends Variance
  case object ContraVariant extends Variance

  class TypeApp(val constructor: ScalaType, val args: List[ScalaType], val variance: Variance = InVariant) extends ScalaType {
    // For now, assume all type parameters are either covariant, contravariant, or invariant.

    private def subArgs(args1: List[ScalaType], args2: List[ScalaType])(implicit lookupType: Identifier => ScalaType) : Result[Boolean] = 
      (args1.size == args2.size).success &&
        (args1.zip(args2) map { 
          case (t1, t2) => 
            (variance == InVariant && t1 == t2) ||
            (variance == CoVariant && t1 :< t2) ||
            (variance == ContraVariant && t1 :> t2)
        } reduce (_ && _))

    override protected def _isSubtypeOf (other: ScalaType)(implicit lookupType: Identifier => ScalaType) = 
      other match {
        case TypeApp(otherConstructor, otherArgs) =>
          constructor :< otherConstructor && subArgs(args, otherArgs)
        case _ => false
      } 

    override def substitute(sub: Substitution) =
      TypeApp(constructor.substitute(sub), args map (_ substitute sub)) setPos pos

    override lazy val freeVars = (args flatMap (_.freeVars) toSet) ++ constructor.freeVars

    def canEqual(that: Any): Boolean = that.isInstanceOf[TypeApp]

    def productArity: Int = 3
    def productElement(n: Int): Any = List(constructor, args, variance)(n)
  }

  object TypeApp {
    def apply(constructor: ScalaType, args: List[ScalaType]): TypeApp = 
      new TypeApp(constructor, args)

    def unapply(x: Any): Option[(ScalaType, List[ScalaType])] = x match {
      case ta: TypeApp =>
        Some((ta.constructor, ta.args))
      case _ => None
    }
  }

  case class SeqTypeConstructor(name: String) extends ScalaType
  case object OptionTypeConstructor extends ScalaType

  case class OptionType(param: ScalaType) extends TypeApp(OptionTypeConstructor, List(param), CoVariant) {

  }

  case class SeqType(name: String, param: ScalaType) extends TypeApp(SeqTypeConstructor(name), List(param), CoVariant) {

  }

  case class TypeProjection(tpe: ScalaType, name: String) extends ScalaType {
    override protected def _isSubtypeOf (other: ScalaType)(implicit lookupType: Identifier => ScalaType) = 
      false // TODO

    override def substitute(sub: Substitution) =
      TypeProjection(tpe.substitute(sub), name) setPos pos

    override lazy val freeVars = tpe.freeVars
  }

  case object ErrorType extends ScalaType {

  }

  // Internal

  case class Trait(name: String, parentOpt: Option[ScalaType]) extends ScalaType {
    override protected def _isSubtypeOf (other: ScalaType)(implicit lookupType: Identifier => ScalaType) = 
      parentOpt map (p => p :< other) getOrElse false

    override val isPositional = true
  }

  case class CaseClass(name: String, params: List[(String, ParamType)], parentOpt: Option[ScalaType]) extends ScalaType {
    override protected def _isSubtypeOf (other: ScalaType)(implicit lookupType: Identifier => ScalaType) = 
      parentOpt map (p => p :< other) getOrElse false

    override def substitute(sub: Substitution) =
      CaseClass(name, params map { case (n, t) => (n, t.substitute(sub)) }, parentOpt) setPos pos

    override val isPositional = true
  }

  object UserType {
    def unapply(t: ScalaType): Option[(String, Option[ScalaType])] = t match {
      case Trait(name, parentOpt) => Some((name, parentOpt))
      case CaseClass(name, _, parentOpt) => Some((name, parentOpt))
      case _ => None
    }
  }

  case class UnknownType(name: Identifier) extends ScalaType {
    override def :<(other: ScalaType)(implicit lookupType: Identifier => ScalaType) = 
      true
    override def :>(other: ScalaType)(implicit lookupType: Identifier => ScalaType) = 
      true
  }

  case class TVar(name: String) extends ScalaType {
    override def substitute(sub: Substitution) = 
      sub(this)

    override lazy val freeVars = Set(this)
  }

  /* Tree definitions */

  case class TreeDefs(defs: List[TreeDef]) extends Section("tree") {

  }

  sealed abstract class TreeDef() extends Tree {
    val name: String
    var tpe: ScalaType = UnTyped
    def getParentType: Option[ScalaType] = 
      parent match {
        case td: TreeDef =>
          Some(td.tpe)
        case _ => 
          None
      }
  }

  case class TreeBranch(name: String, myChildren: List[TreeDef]) extends TreeDef {

  }

  case class TreeLeaf(name: String, params: List[(String, ParamType)]) extends TreeDef {

  }

  /* Settings */

  case class Settings(defs: List[Setting]) extends Section("settings") {

  }

  case class Setting(name: String, value: String) extends Tree {

  }

  /* Declarations */

  case class Declarations(decls: List[Declaration]) extends Section("declarations") {

  }

  case class Declaration(name: String, tpe: ScalaType) extends Tree {

  }
}

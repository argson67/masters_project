package yascc.tree

import scala.util.parsing.input.Positional
//import scala.collection.mutable.{ Set => MSet }

import org.kiama.attribution.Attributable

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

// Types
case UnTyped =>
case FunctionType(args, returnType) =>
case ParamType(tpe, isLazy, isRep) =>
case TupleType(elems) =>
case SimpleType(name) =>
case TypeApp(constructor, args) =>
case TypeProjection(tpe, name) =>

// Internal types

case Trait(name) =>
case CaseClass(name, params) =>
case UnknownType(name) =>
case ErrorType =>

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

  }

  case class Path(prefix: Identifier, name: Name) extends Identifier {

  }

  case class Name(name: String) extends Identifier

  // TODO: Maybe separate these into... separate files? 

  // File stuff

  case class File(sections: Seq[Section]) extends Tree {

  }

  sealed abstract class Section(name: String) extends Tree {

  }

  // Grammar stuff

  case class Grammar(rules: Seq[Rule]) extends Section("grammar") {

  }

  case class Rule(name: String, options: Seq[RuleOption], productions: Seq[Production]) extends Tree {
    var refCount: Int = 0
    var firstSet: Set[Terminal] = Set.empty
    var followSet: Set[Terminal] = Set.empty
  }

  val ErrorRule = Rule("ERROR", Seq.empty, Seq.empty)

  case class RuleOption(name: String, value: Option[String]) extends Tree {

  }

  case class Production(body: ProductionElem, action: Action) extends Tree {

  }

  /* Production elements */

  sealed abstract class ProductionElem extends Tree {

  }

  case class Conjunction(elems: Seq[ProductionElem]) extends ProductionElem {

  }

  case class Disjunction(elems: Seq[ProductionElem]) extends ProductionElem {

  }

  case class Opt(elem: ProductionElem) extends ProductionElem {

  }

  case class Discard(elem: ProductionElem) extends ProductionElem {

  }

  case class Rep(elem: ProductionElem, sep: Option[ProductionElem] = None, strict: Boolean = false) extends ProductionElem {

  }

  case class Label(elem: ProductionElem, label: String) extends ProductionElem {

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

  /* Types */

  // TODO

  sealed abstract class ScalaType() extends Tree {

  }

  case object UnTyped extends ScalaType {

  }

  case class FunctionType(args: Seq[ScalaType], returnType: ScalaType) extends ScalaType {

  }

  case class ParamType(tpe: ScalaType, isLazy: Boolean = true, isRep: Boolean = true) extends ScalaType {

  }

  case class TupleType(elems: Seq[ScalaType]) extends ScalaType {

  }

  case class SimpleType(name: Identifier) extends ScalaType {

  }

  case class TypeApp(constructor: ScalaType, args: Seq[ScalaType]) extends ScalaType {

  }

  case class TypeProjection(tpe: ScalaType, name: String) extends ScalaType {

  }

  case object ErrorType extends ScalaType {

  }

  // Internal

  case class Trait(name: String) extends ScalaType {

  }

  case class CaseClass(name: String, params: Seq[(String, ParamType)]) extends ScalaType {

  }

  case class UnknownType(name: Identifier) extends ScalaType {

  }

  /* Tree definitions */

  case class TreeDefs(defs: Seq[TreeDef]) extends Section("tree") {

  }

  sealed abstract class TreeDef() extends Tree {
    val name: String
  }

  case class TreeBranch(name: String, myChildren: Seq[TreeDef]) extends TreeDef {

  }

  case class TreeLeaf(name: String, params: Seq[(String, ParamType)]) extends TreeDef {

  }

  /* Settings */

  case class Settings(defs: Seq[Setting]) extends Section("settings") {

  }

  case class Setting(name: String, value: String) extends Tree {

  }

  /* Declarations */

  case class Declarations(decls: Seq[Declaration]) extends Section("declarations") {

  }

  case class Declaration(name: String, tpe: ScalaType) extends Tree {

  }
}

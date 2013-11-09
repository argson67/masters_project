package yascc.symtab

import scala.collection.mutable.{ Map => MMap, Set => MSet }
import scala.util.parsing.input.{ Position, NoPosition }

import yascc.Yascc
import yascc.tree.Trees._

import yascc.util.{ Error, Warning, Result }

trait SymbolTable {
  self: Yascc =>

    import yascc.util.Implicits._
    
    abstract class Symbol {
      def name: String
      def pos: Position
      def tpe: ScalaType
    }

    class TermSymbol(val name: String, val pos: Position, val tpe: ScalaType) extends Symbol {
      val isRule: Boolean = false
    }

    object TermSymbol {
      def apply(name: String, pos: Position, tpe: ScalaType) =
        new TermSymbol(name, pos, tpe)

      def unapply(x: Any): Option[(String, Position, ScalaType)] = x match {
        case ts: TermSymbol => Some((ts.name, ts.pos, ts.tpe))
        case _ => None
      }
    }

    val ErrorTermSym = TermSymbol("ERROR", NoPosition, UnTyped)

    case class RuleSymbol(r: Rule, override val tpe: ScalaType = UnTyped) extends TermSymbol(r.name, r.pos, tpe) {
      override val isRule = true

      def setType(newTpe: ScalaType) = RuleSymbol(r, newTpe)
    }

    val ErrorRuleSym = RuleSymbol(ErrorRule)

    case class TypeSymbol(name: String, tpe: ScalaType) extends Symbol {
      val pos = tpe.pos
    }

    val UnTypedSym = TypeSymbol("ERROR", UnTyped)

    private[SymbolTable] val symtab: MMap[String, TermSymbol] = MMap.empty
    private[SymbolTable] val typetab: MMap[String, TypeSymbol] = MMap.empty
    val rules: MSet[Rule] = MSet.empty

    def ruleExists(r: Rule): Boolean = rules contains r
    def registerRule(r: Rule): Unit = rules.add(r)

    // Abstract lookup & define

    def lookupRule(name: String, pos: Position = NoPosition): Result[RuleSymbol] = symtab.get(name) match {
      case Some(rs: RuleSymbol) => rs.success
      case _ => ErrorRuleSym.failure(s"Unresolved reference to rule '$name' @ $pos", pos)
    }

    def lookupValue(name: String, pos: Position = NoPosition): Result[TermSymbol] = symtab.get(name) match {
      case Some(rs: RuleSymbol) => ErrorTermSym.failure(s"Unexpected reference to rule '$name' @ $pos", pos)
      case Some(ts) => ts.success
      case _ => ErrorRuleSym.failure(s"Unresolved reference to value '$name' @ $pos", pos)
    }

    def lookupType(name: String, pos: Position = NoPosition): Result[TypeSymbol] = typetab.get(name) match {
      case Some(ts) => ts.success
      case None => UnTypedSym.failure(s"Unresolved reference to type '$name' @ $pos", pos)
    }

    def defineRule(r: Rule): Result[Unit] = 
      symtab.get(r.name) match {
        case Some(term) => failure(s"Redefinition of '${term.name}' @ ${r.pos}. Previously defined @ ${term.pos}", r.pos)
        case None => 
          symtab(r.name) = RuleSymbol(r)
          OK
      }

    def defineValue(decl: Declaration): Result[Unit] = 
      symtab.get(decl.name) match {
        case Some(term) => failure(s"Redefinition of '${term.name}' @ ${decl.pos}. Previously defined @ ${term.pos}", decl.pos)
        case None => 
          symtab(decl.name) = TermSymbol(decl.name, decl.pos, decl.tpe)
          OK
      }

    def defineType(name: String, tpe: ScalaType): Result[Unit] =
      typetab.get(name) match {
        case Some(oldTpe) => failure(s"Redefinition of type '${name}' @ ${tpe.pos}. Previously defined @ ${oldTpe.pos}", oldTpe.pos)
        case None => 
          typetab(name) = TypeSymbol(name, tpe)
          OK
      }

    def listRules = "Rules:\n" + (rules mkString "\n")
    def listTerms = "Terms:\n" + (symtab mkString "\n")
    def listTypes = "Types:\n" + (typetab mkString "\n")

    def listAll = List(listRules, listTerms, listTypes) mkString "\n\n"
}

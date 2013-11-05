package yascc.symtab

import scala.collection.mutable.{ Map => MMap, Set => MSet }
import scala.util.parsing.input.{ Position, NoPosition }

import yascc.Yascc
import yascc.tree.Trees._

import yascc.util.{ Error, Warning }

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

    case class RuleSymbol(r: Rule, override val tpe: ScalaType = UnTyped) extends TermSymbol(r.name, r.pos, tpe) {
      override val isRule = true

      def setType(newTpe: ScalaType) = RuleSymbol(r, newTpe)
    }

    case class TypeSymbol(name: String, tpe: ScalaType) extends Symbol {
      val pos = tpe.pos
    }

    private[SymbolTable] val symtab: MMap[String, TermSymbol] = MMap.empty
    private[SymbolTable] val typetab: MMap[String, TypeSymbol] = MMap.empty
    private[SymbolTable] val rules: MSet[Rule] = MSet.empty

    def ruleExists(r: Rule): Boolean = rules contains r
    def registerRule(r: Rule): Unit = rules.add(r)

    def lookupTerm(name: String): Option[TermSymbol] = symtab.get(name)
    def lookupType(name: String): Option[TypeSymbol] = typetab.get(name)

    def defineRule(r: Rule): Option[Error] = 
      lookupTerm(r.name) match {
        case Some(term) => Some(s"Redefinition of '${term.name}' @ ${r.pos}. Previously defined @ ${term.pos}")
        case None => 
          symtab(r.name) = RuleSymbol(r)
          None
      }

    def defineValue(decl: Declaration): Option[Error] = 
      lookupTerm(decl.name) match {
        case Some(term) => Some(s"Redefinition of '${term.name}' @ ${decl.pos}. Previously defined @ ${term.pos}")
        case None => 
          symtab(decl.name) = TermSymbol(decl.name, decl.pos, decl.tpe)
          None
      }

    def defineType(name: String, tpe: ScalaType): Option[Error] =
      lookupType(name) match {
        case Some(oldTpe) => Some(s"Redefinition of type '${name}' @ ${tpe.pos}. Previously defined @ ${oldTpe.pos}")
        case None => 
          typetab(name) = TypeSymbol(name, tpe)
          None
      }

    def listRules = "Rules:\n" + (rules mkString "\n")
    def listTerms = "Terms:\n" + (symtab mkString "\n")
    def listTypes = "Types:\n" + (typetab mkString "\n")

    def listAll = List(listRules, listTerms, listTypes) mkString "\n\n"
}

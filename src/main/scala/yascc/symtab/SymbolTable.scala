package yascc.symtab

import scala.collection.mutable.{ Map => MMap, Set => MSet }
import scala.util.parsing.input.{ Position, NoPosition }

import yascc.Yascc
import yascc.tree.Trees._

trait SymbolTable {
  self: Yascc =>
    
    abstract class Symbol {
      def name: String
      def pos: Position
      def tpe: ScalaType
    }

    class TermSymbol(val name: String, val pos: Position, val tpe: ScalaType) extends Symbol {
      val isRule: Boolean = false
    }

    case class RuleSymbol(r: Rule, override val tpe: ScalaType = UnTyped) extends TermSymbol(r.name, r.pos, tpe) {
      override val isRule = true

      def setType(newTpe: ScalaType) = RuleSymbol(r, newTpe)
    }

    case class TypeSymbol(name: String, tpe: ScalaType) extends Symbol {
      val pos = tpe.pos
    }

    private[SymbolTable] val symtab: MMap[String, TermSymbol] = MMap.empty
    private[SymbolTable] val typesym: MMap[String, TypeSymbol] = MMap.empty
    private[SymbolTable] val rules: MSet[Rule] = MSet.empty

    def ruleExists(r: Rule): Boolean = rules contains r
}

package yascc.symtab

import scala.collection.mutable.{ Map => MMap, Set => MSet }
import scala.util.parsing.input.{ Position, NoPosition }

import yascc.Yascc
import yascc.tree.Trees._

import yascc.util.{ Error, Warning, Result }

trait SymbolTable {
  self: Yascc =>

    import yascc.util.Implicits._
    
    abstract class Symbol[T <: ScalaType] {
      def name: String
      def pos: Position
      def tpe: T
    }

    class TermSymbol(val name: String, val pos: Position, val tpe: ScalaType) extends Symbol[ScalaType] {
      val isRule: Boolean = false

      def setType(newTpe: ScalaType) = 
        TermSymbol(name, pos, newTpe)
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

      override def setType(newTpe: ScalaType) = RuleSymbol(r, newTpe)
    }

    val ErrorRuleSym = RuleSymbol(ErrorRule, ErrorType)

    case class TypeSymbol(name: String, tpe: TreeType) extends Symbol[TreeType] {
      val pos = tpe.pos
    }

    val UnTypedSym = TypeSymbol("ERROR", UnTyped)
    def unknownTypeSym(name: Identifier) = TypeSymbol(name.canonicalName, UnTyped)

    private[SymbolTable] val symtab: MMap[String, TermSymbol] = MMap.empty
    private[SymbolTable] val typetab: MMap[String, TypeSymbol] = MMap.empty
    val rules: MSet[Rule] = MSet.empty

    var startRule = ""

    def ruleExists(r: Rule): Boolean = rules contains r
    def registerRule(r: Rule): Unit = rules.add(r)

    // TODO: Abstract lookup & define

    def lookupRule(name: String, pos: Position = NoPosition): Result[RuleSymbol] = symtab.get(name) match {
      case Some(rs: RuleSymbol) => rs.success
      case _ => ErrorRuleSym.failure(s"Unresolved reference to rule '$name' @ $pos", pos)
    }

    def lookupValue(name: String, pos: Position = NoPosition): Result[TermSymbol] = symtab.get(name) match {
      case Some(rs: RuleSymbol) => ErrorTermSym.failure(s"Unexpected reference to rule '$name' @ $pos", pos)
      case Some(ts) => ts.success
      case _ => ErrorRuleSym.failure(s"Unresolved reference to value '$name' @ $pos", pos)
    }

    def lookupType(name: Identifier, pos: Position = NoPosition): Result[TypeSymbol] = name match {
      case p: Path =>
        unknownTypeSym(name).success.addWarning(s"Unknown type '$name'")
      case Name(n) =>
        typetab.get(n) match {
          case Some(ts) => ts.success
          case None => unknownTypeSym(name).success.addWarning(s"Unknown type '$n'")
          //UnTypedSym.failure(s"Unresolved reference to type '$name' @ $pos", pos)
        }
    }

    def setType(name: String, tpe: ScalaType): Result[Unit] =
      symtab.get(name) match {
        case Some(s) => 
          symtab(name) = s.setType(tpe)
          OK
        case None => 
          failure(s"Unresolved reference to '$name'")
      }

    def getType(name: String, pos: Position = NoPosition): Result[ScalaType] = 
      symtab.get(name) match {
        case Some(s) => s.tpe.success
        case None => UnTyped.failure(s"Unresolved reference to '$name' @ $pos", pos)
      }

    def defineRule(r: Rule): Result[Unit] = 
      symtab.get(r.name) match {
        case Some(term) => failure(s"Redefinition of '${term.name}' @ ${r.pos}. Previously defined @ ${term.pos}", r.pos)
        case None => 
          symtab(r.name) = RuleSymbol(r)
          OK
      }

    def defineValue(decl: Declaration): Result[Unit] = 
      defineValue(decl.name, decl.pos, decl.tpe)

    def defineValue(name: String, pos: Position, tpe: ScalaType): Result[Unit] = 
      symtab.get(name) match {
        case Some(term) => failure(s"Redefinition of '${term.name}' @ ${pos}. Previously defined @ ${pos}", pos)
        case None => 
          symtab(name) = TermSymbol(name, pos, tpe)
          OK
      }

    def defineType(name: String, tpe: TreeType): Result[Unit] =
      typetab.get(name) match {
        case Some(oldTpe) => failure(s"Redefinition of type '${name}' @ ${tpe.pos}. Previously defined @ ${oldTpe.pos}", oldTpe.pos)
        case None => 
          typetab(name) = TypeSymbol(name, tpe)
          OK
      }

    def listRules = "Rules:\n" + (rules mkString "\n")
    def listTerms = "Terms:\n" + symtab // + (symtab mkString "\n")
    def listTypes = "Types:\n" + (typetab mkString "\n")

    def listAll = List(listRules, listTerms, listTypes) mkString "\n\n"

    def numRules = symtab.values.filter(_.isRule).size
    def numDefs = symtab.values.filter(s => !s.isRule && !(typetab.contains(s.name))).size
    def numTrees = typetab.size

    def allTypes = typetab.values map (_.tpe)
    def allValues = symtab.values.filter(!_.isRule) withFilter(s => !(typetab.contains(s.name))) map (s => (s.name, s.tpe)) 

    def _allDecls = symtab.values.filter(!_.isRule)
}

package yascc.tree

import org.kiama.output.PrettyPrinter
import Trees._

object TreePrinter extends PrettyPrinter {
  def apply(tree: Tree) = pretty(printTree(tree), 120)

  private[TreePrinter] def vsep2(ds: Seq[Doc]): Doc =
    ssep(ds, line <> line)

  private[TreePrinter] def printTree(tree: Tree): Doc = {
    def printRuleOptions(options: Seq[RuleOption]): Doc = 
      if (options.isEmpty) {
        empty
      } else {
        space <> brackets(lsep(options map printTree, comma))
      }

    def printRep(elem: ProductionElem, sep: Option[ProductionElem], strict: Boolean): Doc = {
      val op = if (strict) "+" else "*"
      sep.map(s => printTree(elem) <+> angles(op) <+> printTree(s)).getOrElse(parens(printTree(elem)) <> op)
    }

    tree match {
      // Identifiers
      case Path(prefix, name) =>
        printTree(prefix) <> dot <> printTree(name)

      case Name(name) =>
        name

      case File(sections) =>
        vsep2(sections map printTree)

      // Grammar
      case Grammar(rules) =>
        "#grammar" <@> vsep2(rules map printTree)

      case Rule(name, options, productions) =>
        name <+> "::=" <> printRuleOptions(options) <+> nest(lsep2(productions map printTree, "|") <> ";", name.length + 3)
      case RuleOption(name, value) =>
        value.map(v => name <+> "=" <+> dquotes(v)).getOrElse(name)
      case Production(body, action) =>
        printTree(body) <+> "->" </> nest(printTree(action))

      // Production elements
      case Conjunction(elems) =>
        hsep(elems map printTree)
      case Disjunction(elems) =>
        nest(lsep(elems map printTree, softline <> "|"))
      case Opt(elem) =>
        parens(printTree(elem)) <> "?"
      case Discard(elem) =>
        brackets(printTree(elem))
      case Rep(elem, sep, strict) =>
        printRep(elem, sep, strict)
      case Label(elem, label) =>
        printTree(elem) <+> "<?>" <+> dquotes(label)
      case NonTerminal(name) => name

      // Terminals
      case NumberLiteral(num) =>
        value(num)
      case StringLiteral(str) =>
        dquotes(str)
      case RegexLiteral(regex) =>
        enclose("r\"", regex, "\"")
      case CharLiteral(c) =>
        squotes(c)
      case Epsilon => "\u03B5"

      // Actions
      case FunctionAction(fun) =>
        printTree(fun)
      case CustomAction(code, tpe) =>
        "{%" <@> code <@> "%}" <+> colon <+> printTree(tpe)

      // Types
      case UnTyped => "*untyped*"
      case FunctionType(args, returnType) =>
        val as = if (args.size == 1) printTree(args(0)) else parens(lsep(args map printTree, comma))
        as <+> "=>" <+> printTree(returnType)
      case ParamType(tpe, isLazy, isRep) =>
        val l: Doc = if (isLazy) "=>" <> space else empty
        val r: Doc = if (isRep) "*" else empty
        l <> printTree(tpe) <> r
      case TupleType(elems) =>
        parens(lsep(elems map printTree, comma))
      case SimpleType(name) =>
        printTree(name)
      case TypeApp(constructor, args) =>
        printTree(constructor) <> parens(lsep(args map printTree, comma))
      case TypeProjection(tpe, name) =>
        printTree(tpe) <> "#" <> name

      // Internal types

      case Trait(name) => "Trait" <> parens(name)
      case CaseClass(name, params) => "CaseClass" <> parens(name)
      case UnknownType(name) => "Unknown" <> parens(printTree(name))
      case ErrorType => "ERROR"

      // Tree defs
      case TreeDefs(defs) =>
        "#tree" <@> vsep2(defs map printTree)
      case TreeBranch(name, myChildren) =>
        name <+> braces(nest(vsep(myChildren map printTree)))
      case TreeLeaf(name, params) =>
        name <> parens(lsep(params map {
          case (name, tpe) => name <+> ":" <+> printTree(tpe)
        }, comma))

      // Settings
      case Settings(defs) =>
        "#settings" <@> vsep(defs map printTree)
      case Setting(name, value) =>
        name <+> "=" <+> dquotes(value)

      // Declarations
      case Declarations(decls) =>
        "#declarations" <@> vsep(decls map printTree)
      case Declaration(name, tpe) =>
        name <+> colon <+> printTree(tpe)
    }

  }
}

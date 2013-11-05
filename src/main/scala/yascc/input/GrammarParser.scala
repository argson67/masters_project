package yascc.input

import yascc.tree.Trees._

trait GrammarParser {
  self: FileParser =>

    // A grammar is a collection of rules
    // Entry point
    lazy val grammar = rep(rule) ^^ Grammar.apply

    // A rule is an identifier, followed by a an optional (haha) set of options and a non-empty list of productions

    private[GrammarParser] lazy val rule: PackratParser[Rule] = 
      positioned(identifier ~ opt(ruleOptions) ~ ("::=" ~> rep1sep(production, "|") <~ ";") ^^ {
      case name ~ options ~ prods => Rule(name, options.getOrElse(Nil), prods)
    })

    private[GrammarParser] lazy val ruleOptions = brackets(rep1sep(ruleOption, ","))

    private[GrammarParser] lazy val ruleOption: PackratParser[RuleOption] = positioned(identifier ~ opt("=" ~> string) ^^ {
      case name ~ value => RuleOption(name, value)
    })

    // A production is a conjunction of terms, followed by an action

    private[GrammarParser] lazy val production: PackratParser[Production] = positioned(conjunction ~ ("->" ~> action) ^^ {
      case t ~ a => Production(t, a)
    })

    private[GrammarParser] lazy val action: PackratParser[Action] = (
      identifier ^^ (i => FunctionAction(Name(i)))
      | customAction
    )

    private[GrammarParser] lazy val customAction: PackratParser[Action] = 
      "(\\{%([^%]|[\\r\\n]|(%+([^%\\}]|[\\r\\n])))*%\\})".r ~ (":" ~> scalaType) ^^ { 
        case code ~ tpe => CustomAction(code.toString.slice(2, code.size - 2), tpe) 
    }

    private[GrammarParser] lazy val disjunction: PackratParser[Disjunction] = 
      positioned(rep1sep(conjunction, "|") ^^ Disjunction.apply)

    private[GrammarParser] lazy val conjunction: PackratParser[Conjunction] = 
      positioned(rep1(repSepTerm) ^^ Conjunction.apply)

    // TODO: Chained terms

    private[GrammarParser] lazy val repSepTerm: PackratParser[ProductionElem] = positioned(
      optionalTerm ~ ("<+>" ^^^ true | "<*>" ^^^ false) ~ optionalTerm ^^ {
        case t ~ force ~ sep => Rep(t, Some(sep), force)
      }
      | optionalTerm)

    private[GrammarParser] lazy val optionalTerm: PackratParser[ProductionElem] = positioned(
      repTerm ~ opt("?") ^^ {
        case t ~ Some(_) => Opt(t)
        case t ~ None => t
      })

    private[GrammarParser] lazy val repTerm: PackratParser[ProductionElem] = positioned(
      labeledTerm ~ ("+" ^^^ true | "*" ^^^ false) ^^ {
        case t ~ force => Rep(t, None, force)
      }
      | labeledTerm)

    private[GrammarParser] lazy val labeledTerm: PackratParser[ProductionElem] = positioned(
      simpleTerm ~ ("<?>" ~> string) ^^ {
        case t ~ label => Label(t, label)
      }
      | simpleTerm
      )

    private[GrammarParser] lazy val simpleTerm: PackratParser[ProductionElem] = positioned(
      braces(disjunction) ^^ Discard.apply
      | parens(disjunction) 
      | identifier ^^ Name.apply
      | string ^^ StringLiteral.apply
      | regex ^^ RegexLiteral.apply
      | number
      )
}

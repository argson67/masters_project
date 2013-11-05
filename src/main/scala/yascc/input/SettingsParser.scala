package yascc.input

import scala.util.parsing.combinator.{ PackratParsers, RegexParsers }

import yascc.tree.Trees._

trait SettingsParser {
  self: FileParser =>

    lazy val settings: PackratParser[Settings] = rep1(setting) ^^ Settings.apply

    private[SettingsParser] lazy val setting: PackratParser[Setting] = identifier ~ ("=" ~> string) ^^ {
      case name ~ value => Setting(name, value)
    }
}

package yascc.util

import scala.util.parsing.input.{Position, NoPosition}

import scala.language.implicitConversions

object Implicits {
  implicit class ToResult[T](obj: T) {
    def success: Result[T] = Success(obj)

    def failure(msg: String, pos: Position = NoPosition): Failure[T] = Failure(obj, Seq(Error(msg, pos)))
  }

  def error(msg: String, pos: Position = NoPosition): FatalError = FatalError(Seq(Error(msg, pos)))

  implicit def str2error(str: String): Error = Error(str)
  implicit def str2warning(str: String): Warning = Warning(str)
}

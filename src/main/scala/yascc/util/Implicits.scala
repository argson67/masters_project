package yascc.util

import scala.util.parsing.input.{Position, NoPosition}

object Implicits {
  implicit class ToResult[T](obj: T) {
    def success: Result[T] = Success(obj)

    def failure(msg: String, pos: Position = NoPosition): Failure[T] = Failure(obj, Seq(Error(msg, pos)))
  }

  def error(msg: String, pos: Position = NoPosition): FatalError = FatalError(Seq(Error(msg, pos)))
}

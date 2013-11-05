package yascc.util

import scala.util.parsing.input.{Position, NoPosition}

case class NoResultException(e: FatalError) extends Exception {

}

sealed abstract class Result[+T] {
  def isSuccess: Boolean
  def isFailure: Boolean = !isSuccess
  def isFatal: Boolean

  def hasWarnings: Boolean = !warnings.isEmpty

  def warnings: Seq[Warning]
  def errors: Seq[Error]

  def addError(error: Error, fatal: Boolean = false): Result[_]
  def addWarning(warning: Warning): Result[T]

  def foreach[A](f: (T) => A): Unit
  def map[A](f: (T) => A): Result[A]
  def flatMap[A](f: (T) => Result[A]): Result[A]

  def :+[A](other: Result[A]) = other flatMap (_ => this)
  def +:[A](other: Result[A]) = flatMap (_ => other)

  def get: T
}

object Result {
  def chain[A](seq: Seq[A])(f: A => Result[Unit]) = 
    seq.foldLeft(unit) {
      (soFar, e) => soFar flatMap (_ => f(e))
    }

  def chainAny[A, B, C](start: Result[A], seq: Seq[B])(f: B => Result[C]) =
    seq.foldLeft(start) {
      (soFar, e) => soFar :+ (f(e))
    }

  val unit: Result[Unit] = Success(())
}

case class Success[T](value: T, warnings: Seq[Warning] = Nil) extends Result[T] {
  val errors = Nil
  val isSuccess = true
  val isFatal = false

  def addError(error: Error, fatal: Boolean = false) =
    if (fatal) FatalError(Seq(error), warnings) else Failure(value, Seq(error), warnings)
  def addWarning(warning: Warning) =
    Success(value, warning +: warnings)

  def foreach[A](f: (T) => A) { f(value) }
  def map[A](f: (T) => A) = Success(f(value), warnings)
  def flatMap[A](f: (T) => Result[A]) = f(value)

  val get = value
}

sealed abstract class NoSuccess[T](errors: Seq[Error], warnings: Seq[Warning]) extends Result[T] {
  val isSuccess = false
}

case class Failure[T](value: T, errors: Seq[Error], warnings: Seq[Warning] = Nil) extends NoSuccess[T](errors, warnings) {
  val isFatal = false

  def addError(error: Error, fatal: Boolean = false) =
    if (fatal) FatalError(error +: errors, warnings) else Failure(value, error +: errors, warnings)
  def addWarning(warning: Warning) = 
    Failure(value, errors, warning +: warnings)

  def foreach[A](f: (T) => A) { f(value) }
  def map[A](f: (T) => A) = Failure(f(value), errors, warnings)
  def flatMap[A](f: (T) => Result[A]) = f(value) match {
    case Success(v, warns) => Failure(v, errors, warns ++ warnings)
    case Failure(v, errs, warns) => Failure(v, errs ++ errors, warns ++ warnings)
    case FatalError(errs, warns) => FatalError(errs ++ errors, warns ++ warnings)
  }

  val get = value
}

case class FatalError(errors: Seq[Error], warnings: Seq[Warning] = Nil) extends NoSuccess[Nothing](errors, warnings) {
  val isFatal = true

  def addError(error: Error, fatal: Boolean = false) =
    FatalError(error +: errors, warnings)
  def addWarning(warning: Warning) = 
    FatalError(errors, warning +: warnings)

  def foreach[A](f: (Nothing) => A) = ()
  def map[A](f: (Nothing) => A) = this
  def flatMap[A](f: (Nothing) => Result[A]) = this

  def get = throw NoResultException(this)
}

case class Warning(msg: String, pos: Position = NoPosition)

case class Error(msg: String, pos: Position = NoPosition)

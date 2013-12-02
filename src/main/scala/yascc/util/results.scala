package yascc.util

import scala.util.parsing.input.{Position, NoPosition}

case class NoResultException(e: FatalError) extends Exception {

}

// this wants to be a Writer monad, basically

sealed abstract class Result[+T] {
  def isSuccess: Boolean
  def isFailure: Boolean = !isSuccess
  def isFatal: Boolean

  def hasWarnings: Boolean = !warnings.isEmpty

  def warnings: Seq[Warning]
  def errors: Seq[Error]

  def addError(error: Error, fatal: Boolean = false): Result[_ <: T]
  def addWarning(warning: Warning): Result[T]

  def foreach[A](f: (T) => A): Unit
  def map[A](f: (T) => A): Result[A]
  def flatMap[A](f: (T) => Result[A]): Result[A]
  def filter(f: (T) => Boolean): Result[T]

  def :+[A](other: Result[A]): Result[T] = other flatMap (_ => this)
  def +:[A](other: Result[A]): Result[T] = :+(other)

  def get: T

  def toUnit: Result[Unit] = map (_ => ())

  def printErrors: String = 
    (errors.distinct map (e => s"[\033[31merror\033[39m] ${e.msg} \n ${e.pos.longString}") mkString "\n") ++
    (warnings.distinct map (w => s"[\033[33mwarning\033[39m] ${w.msg}") mkString "\n")
}

object Result {
  // TODO: rewrite

  def chain[A](seq: Seq[A])(f: A => Result[Unit]) = 
    seq.foldLeft(unit) {
      (soFar, e) => soFar flatMap (_ => f(e))
    }

  def chainAny[A, B, C](start: Result[A], seq: Seq[B])(f: B => Result[C]) =
    seq.foldLeft(start) {
      (soFar, e) => soFar :+ (f(e))
    }

  def sequence[A](seq: Seq[Result[A]]): Result[Seq[A]] = seq.foldLeft(Success(Seq.empty[A]): Result[Seq[A]]) {
      (soFar, r) => 
        for ( res <- soFar
            ; e <- r
            ) yield res :+ e
  }

  def fromOption[A](opt: Option[Result[A]]): Result[Option[A]] = 
    opt match {
      case Some(a) => a map (Some(_))
      case None => Success(None)
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
  def flatMap[A](f: (T) => Result[A]) = 
    warnings.foldLeft(f(value)) { (v, w) => v.addWarning(w) }

  def filter(f: (T) => Boolean): Result[T] = 
    if (f(value)) this else FatalError(errors, warnings)

  val get = value

  //println(s"**** CREATING NEW SUCCESS($value, $warnings)")
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
  def filter(f: (T) => Boolean): Result[T] = 
    if (f(value)) this else FatalError(errors, warnings)

  val get = value
}

// UResult = Usable result, i.e. result that carries some kind of value
object UResult {
  def unapply[T](x: Result[T]) = x match {
    case Success(v, _) => Some(v)
    case Failure(v, _, _) => Some(v)
    case _ => None
  }
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
  def filter(f: (Nothing) => Boolean): Result[Nothing] = this

  def get = throw NoResultException(this)
}

case class Warning(msg: String, pos: Position = NoPosition)

case class Error(msg: String, pos: Position = NoPosition)

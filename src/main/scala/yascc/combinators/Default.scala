package yascc.combinators

object Defaults {
  trait Default[+T] {
    def get: T
  }

  implicit val DefaultUnit = new Default[Unit] {
    val get = ()
  }

  implicit val DefaultBoolean = new Default[Boolean] {
    val get = false
  }

  implicit val DefaultByte = new Default[Byte] {
    val get = 0.toByte
  }

  implicit val DefaultChar = new Default[Char] {
    val get = 0.toChar
  }

  implicit val DefaultShort = new Default[Short] {
    val get = 0.toShort
  }

  implicit val DefaultInt = new Default[Int] {
    val get = 0
  }

  implicit val DefaultLong = new Default[Long] {
    val get = 0l
  }

  implicit val DefaultFloat = new Default[Float] {
    val get = 0.0f
  }

  implicit val DefaultDouble = new Default[Double] {
    val get = 0.0
  }

  implicit def DefaultAnyRef[T >: Null <: AnyRef] = new Default[T] {
    val get = null
  }
}

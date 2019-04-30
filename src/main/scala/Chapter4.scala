object Chapter4 {

  trait Option[+A] {
    def map[B](f: A => B): Option[B] = this match {
      case None => None
      case Some(a) => Some(f(a))
    }

    def flatMap[B](f: A => Option[B]): Option[B] = this match {
      case None => None
      case Some(a) => f(a)
    }

    def getOrElse[B >: A](default: => B): B = this match {
      case None => default
      case Some(b) => b
    }

    def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
      case None => ob
      case Some(a) => Some(a)
    }

    def filter(f: A => Boolean): Option[A] = this match {
      case Some(a) if f(a) => this
      case _ => None
    }

    //def variance(xs: Seq[Double]): Option[Double]
  }
  object Option {
    def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

    def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
      for {
        aa <- a
        bb <- b
      } yield f(aa, bb)
    }

    def sequence[A](a: List[Option[A]]): Option[List[A]] = {
      traverse(a)(x => x)
    }

    def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
      a match {
        case Nil => Some(Nil)
        case x :: xs => map2(f(x), traverse(xs)(f))(_ :: _)
      }
    }
  }

  def Try[A](a: => A): Option[A] = {
    try Some(a)
    catch { case e: Exception => None }
  }

  case class Some[+A](get: A) extends Option[A]
  case object None extends Option[Nothing]

  sealed trait Either[+E, +A] {
    def map[B](f: A => B): Either[E, B] = this match {
      case Right(value) => Right(f(value))
      case Left(e) => Left(e)
    }
    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = {
      this match {
        case Left(value) => Left(value)
        case Right(value) => f(value)
      }
    }

    def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = {
      this match {
        case Left(_) => b
        case Right(a) => Right(a)
      }
    }

    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
      for {
        aa <- this
        bb <- b
      } yield f(aa, bb)
    }
  }

  object Either {
    def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = {
      traverse(es)(x => x)
    }

    def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
      as match {
        case Nil => Right(Nil)
        case h :: t => (f(h) map2 traverse(t)(f))(_ :: _)
      }
    }
  }

  case class Left[+E](value: E) extends Either[E, Nothing]
  case class Right[+A](value: A) extends Either[Nothing, A]

}

package uk.co.odinconsultants.agentsy
import cats.{Monad, Defer}
import cats.effect.kernel.Concurrent

import scala.annotation.tailrec

object Effects {
  case class MyIO[A](unsafeRun: () => A)
  object MyIO {
    def create[A](a: => A): MyIO[A] = MyIO(() => a)
  }

  given ioMonad: Monad[MyIO] with {
    override def pure[A](a: A): MyIO[A]                                    =
      MyIO(() => a)
    override def flatMap[A, B](ma: MyIO[A])(f: A => MyIO[B]): MyIO[B]      =
      MyIO(() => f(ma.unsafeRun()).unsafeRun())
    override def tailRecM[A, B](a: A)(f: A => MyIO[Either[A, B]]): MyIO[B] = {
      @tailrec def go(a: A)(f: A => MyIO[Either[A, B]]): MyIO[B] = {
        f(a).unsafeRun() match {
          case Left(a)  => go(a)(f)
          case Right(x) => MyIO.create(x)
        }
      }
      go(a)(f)
    }
  }

  given ioDefer: Defer[MyIO] with {
    override def defer[A](fa: => MyIO[A]): MyIO[A] = MyIO(() => fa.unsafeRun())
  }

}

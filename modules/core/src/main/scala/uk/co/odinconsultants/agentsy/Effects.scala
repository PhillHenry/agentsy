package uk.co.odinconsultants.agentsy
import cats.{Monad, Defer}
import cats.effect.kernel.Concurrent

import scala.annotation.tailrec

object Effects {
  case class StateEffect[A](unsafeRun: () => A)
  object StateEffect {
    def create[A](a: => A): StateEffect[A] = StateEffect(() => a)
  }

  given ioMonad: Monad[StateEffect] with {
    override def pure[A](a: A): StateEffect[A]                                    =
      StateEffect(() => a)
    override def flatMap[A, B](ma: StateEffect[A])(f: A => StateEffect[B]): StateEffect[B]      =
      StateEffect(() => f(ma.unsafeRun()).unsafeRun())
    override def tailRecM[A, B](a: A)(f: A => StateEffect[Either[A, B]]): StateEffect[B] = {
      @tailrec def go(a: A)(f: A => StateEffect[Either[A, B]]): StateEffect[B] = {
        f(a).unsafeRun() match {
          case Left(a)  => go(a)(f)
          case Right(x) => StateEffect.create(x)
        }
      }
      go(a)(f)
    }
  }

  given ioDefer: Defer[StateEffect] with {
    override def defer[A](fa: => StateEffect[A]): StateEffect[A] = StateEffect(() => fa.unsafeRun())
  }

}

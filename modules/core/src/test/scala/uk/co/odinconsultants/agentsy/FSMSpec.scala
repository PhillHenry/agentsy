package uk.co.odinconsultants.agentsy

import org.scalatest.*
import org.scalatest.matchers.Matcher
import cats.{Applicative, Id, Monad, Defer}
import cats.syntax.all.*

import java.util.concurrent.atomic.AtomicInteger

class FSMSpec extends wordspec.AnyWordSpec {

  "Simple FSM" should {
    "transition state" in {
      val model        = new StateModel[Id]
      val initialState = FSM(model.run)
      assert(initialState.run(100, 1)._1 == 101)
    }
  }

  trait HealthCareFixture[T[_]: Monad, Output] {
    type StateTransition = (HealthCareDemand, Float) => T[(HealthCareDemand, Output)]
    val initialEmergencyCount = 1
    val initialAmbulanceCount = 1
    val initialGPCount        = 1
    val initialState          = HealthCareDemand(initialGPCount, initialEmergencyCount, initialGPCount)
  }

  "Monadic health care FSM" should {
    "increase counts according to monadic state transition" in {
      import Effects.*
      val fixture                      = new HealthCareFixture[MyIO, Unit] {}
      import fixture.*
      val model: HealthCareModel[MyIO] = new HealthCareModel[MyIO]
      val seeds                        = List(model.typicalWalkInSeed, model.typicalAmbulanceSeed, model.typicalGPSeed)

      val (finalState, outputs) =
        seeds.foldLeft((initialState, MyIO(() => println("Started")))) { case (acc, seed) =>
          val (state: HealthCareDemand, output: MyIO[Unit]) = acc
          val myIO: MyIO[(HealthCareDemand, MyIO[Unit])]    = model.transition(state, seed)
          val (newState, newOutput)                         = myIO.unsafeRun()
          (newState, newOutput *> output)
        }
      outputs.unsafeRun()
      assert(finalState.emergency == initialEmergencyCount + 1)
    }
  }

  "Health care FSM" should {
    "increase counts according to simple state transition" in {
      given hack: Defer[Id] with {
        override def defer[A](fa: => Id[A]): Id[A] = fa
      }
      val fixture       = new HealthCareFixture[Id, Unit] {}
      import fixture.*
      val model: HealthCareModel[Id] = new HealthCareModel[Id]
      val (newState, _) = model.transition(initialState, model.walkInThreshold / 2)
      assert(newState.emergency == initialEmergencyCount + 1)
    }
  }

}

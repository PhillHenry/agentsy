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
          val myIO: MyIO[(HealthCareDemand, MyIO[Unit])]    = model.transitionEffectfully(state, seed)
          val (newState, newOutput)                         = myIO.unsafeRun()
          (newState, newOutput *> output)
        }
      println("=== About to run outputs...")
      outputs.unsafeRun()
      println("=== Finished running outputs")
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
      val (newState, _) = model.transitionEffectfully(initialState, model.walkInThreshold / 2)
      assert(newState.emergency == initialEmergencyCount + 1)
    }
  }

}

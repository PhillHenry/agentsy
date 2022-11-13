package uk.co.odinconsultants.agentsy

import org.scalatest.*
import org.scalatest.matchers.Matcher
import cats.{Applicative, Id, Monad}
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
    val model: HealthCareModel[T] = new HealthCareModel[T]
    type StateTransition = (HealthCareDemand, Float) => T[(HealthCareDemand, Output)]
    val initialEmergencyCount = 1
    val initialAmbulanceCount = 1
    val initialGPCount        = 1
    val emergency             = AtomicInteger(initialEmergencyCount)
    val ambulance             = AtomicInteger(initialAmbulanceCount)
    val gp                    = AtomicInteger(initialGPCount)
  }

  "Monadic health care FSM" should {
    "increase counts according to monadic state transition" in {
      import Effects.*
      val fixture = new HealthCareFixture[MyIO, Unit] {}
      import fixture.*
      val seeds   = List(model.typicalWalkInSeed, model.typicalAmbulanceSeed, model.typicalGPSeed)

      // (HealthCareDemand, Float) => T[(HealthCareDemand, Output)]
      val transition: (HealthCareDemand, Float) => MyIO[(HealthCareDemand, MyIO[Unit])] =
        model.transition
      val initialState                                                                  = HealthCareDemand(emergency.get, ambulance.get, gp.get)
      val transitions: (HealthCareDemand, MyIO[Unit])                                   =
        seeds.foldLeft((initialState, MyIO(() => println("Started")))) { case (acc, seed) =>
          val (state: HealthCareDemand, output: MyIO[Unit]) = acc
          val myIO: MyIO[(HealthCareDemand, MyIO[Unit])]    = transition(state, seed)
          val (newState, newOutput)                         = myIO.unsafeRun()
          (newState, output *> newOutput)
        }
      assert(transitions._1.emergency == initialEmergencyCount + 1)
    }
  }

  "Health care FSM" should {
    "increase counts according to simple state transition" in {
      val fixture       = new HealthCareFixture[Id, () => Int] {}
      import fixture.*
      val initialState  = HealthCareDemand(emergency.get, ambulance.get, gp.get)
      val (newState, _) = model.transition(initialState, model.walkInThreshold / 2)
      assert(newState.emergency == initialEmergencyCount + 1)
    }
  }

}

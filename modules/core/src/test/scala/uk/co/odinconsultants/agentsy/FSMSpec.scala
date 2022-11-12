package uk.co.odinconsultants.agentsy

import org.scalatest.*
import org.scalatest.matchers.Matcher
import cats.{Id, Monad}

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
    val model: HealthCareModel[T, Output] = new HealthCareModel[T, Output]
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
      val fixture = new HealthCareFixture[MyIO, MyIO[Float]] {}
      import fixture.*
//      val transition: StateTransition = model.initialState(
    }
  }

  "Health care FSM" should {
    "increase counts according to simple state transition" in {
      val fixture                     = new HealthCareFixture[Id, () => Int] {}
      import fixture.*
      val transition: StateTransition = model.initialState(
        Id(() => emergency.incrementAndGet()),
        Id(() => ambulance.incrementAndGet()),
        Id(() => gp.incrementAndGet()),
      )
      val initialState                = HealthCareDemand(emergency.get, ambulance.get, gp.get)
      val (newState, output)          = transition(initialState, model.walkInThreshold / 2)
      output()
      assert(emergency.get() == 2)
    }
  }

}

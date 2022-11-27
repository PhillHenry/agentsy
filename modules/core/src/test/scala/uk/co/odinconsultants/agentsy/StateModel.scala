package uk.co.odinconsultants.agentsy

import cats.effect.IOApp
import cats.{Applicative, Defer, Monad}
import shapeless.ops.coproduct.FlatMap
import cats.syntax.all.*
import scala.util.Random

trait Model[T[_]] {
  type State
  type Input
  type Output
  type StateTransition = (State, Input) => T[(State, Output)]
}

case class HealthCareDemand(gp: Int, emergency: Int, ambulance: Int) {
  def total: Long = gp + emergency + ambulance
}

class HealthCareModel[T[_]: Monad: Defer] {
  type State = HealthCareDemand
  type Input = Float
  val walkInThreshold      = 0.4f
  val ambulanceThreshold   = 0.5f
  val typicalWalkInSeed    = 0.39f
  val typicalAmbulanceSeed = 0.49f
  val typicalGPSeed        = 0.51f
  val maxAmbulance         = 200
  val DoNothing: T[Unit]   = Applicative[T].pure(())

  def deferPrint(msg: String): T[Unit] = Defer[T].defer {
    println(msg)
    DoNothing
  }

  val transitionEffectfully: (HealthCareDemand, Float) => T[(HealthCareDemand, T[Unit])] =
    (state, input) =>
      Applicative[T].pure {
        if (input < walkInThreshold) // walk-in
          (
            state.copy(emergency = state.emergency + 1),
            deferPrint("Emergency")
          )
        else if (input < ambulanceThreshold)
            if (state.ambulance >= maxAmbulance) (
              state.copy(emergency = state.emergency + 1),
              deferPrint("No ambulances. Patient walks to A&E")
            ) else (
              state.copy(ambulance = state.ambulance + 1),
              deferPrint("Ambulance")
            )
        else
          (
            state.copy(gp = state.gp + 1),
            deferPrint("GP")
          )
      }

  val transition: (HealthCareDemand, Float) => (HealthCareDemand, T[Unit]) = (state, input) =>
    if (input < walkInThreshold)
      (state.copy(emergency = state.emergency + 1), DoNothing)
    else if (input < ambulanceThreshold)
      (state.copy(ambulance = state.ambulance + 1), DoNothing)
    else (state.copy(gp = state.gp + 1), DoNothing)
}

class StateModel[T[_]: Applicative] extends Model[T] {
  type State           = Long
  type Input           = Int
  type Output          = Unit
  type StateTransition = (State, Input) => T[(State, Output)]
  val run: StateTransition = (state, input) =>
    Applicative[T].pure {
      val newState: Long = state + input
      (newState, println(s"$state - $input -> $newState"))
    }
}

trait HealthCareFixture[T[_]: Monad, Output] {
  type StateTransition = (HealthCareDemand, Float) => T[(HealthCareDemand, Output)]
  val initialEmergencyCount = 0
  val initialAmbulanceCount = 0
  val initialGPCount        = 0
  val initialState          = HealthCareDemand(initialGPCount, initialEmergencyCount, initialGPCount)
}

object StateModel {

  def main(args: Array[String]): Unit = {
    import Effects.*
    val fixture                      = new HealthCareFixture[StateEffect, Unit] {}
    import fixture.*
    val model: HealthCareModel[StateEffect] = new HealthCareModel[StateEffect]
    val n                            = 100000000
    val seeds                        = (1 to n).map(_ => Random.nextFloat())
    println(s"Running $n times")
    val (finalState, outputs)        =
      seeds.foldLeft((initialState, StateEffect(() => println("Started")))) { case (acc, seed) =>
        val (state: HealthCareDemand, output: StateEffect[Unit]) = acc
        val (newState, newOutput)                         = model.transition(state, seed)
//        (newState, newOutput *> output)  // this *> appears to create a new MyIO
        (newState, model.DoNothing) // much more efficient
      }
    println(finalState)
    println(s"Total ${finalState.total}")
  }

}

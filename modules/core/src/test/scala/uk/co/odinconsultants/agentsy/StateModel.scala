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
  val DoNothing: T[Unit]   = Applicative[T].pure(())

  val transitionEffectfully: (HealthCareDemand, Float) => T[(HealthCareDemand, T[Unit])] =
    (state, input) =>
      Applicative[T].pure {
        if (input < walkInThreshold) // walk-in
          (
            state.copy(emergency = state.emergency + 1),
            Defer[T].defer {
              println("Emergency")
              DoNothing
            },
          )
        else if (input < ambulanceThreshold)
          (
            state.copy(ambulance = state.ambulance + 1),
            Defer[T].defer {
              println("Ambulance")
              DoNothing
            },
          )
        else
          (
            state.copy(gp = state.gp + 1),
            Defer[T].defer {
              println("GP")
              DoNothing
            },
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
  val initialEmergencyCount = 1
  val initialAmbulanceCount = 1
  val initialGPCount        = 1
  val initialState          = HealthCareDemand(initialGPCount, initialEmergencyCount, initialGPCount)
}

object StateModel {

  def main(args: Array[String]): Unit = {
    import Effects.*
    val fixture                      = new HealthCareFixture[MyIO, Unit] {}
    import fixture.*
    val model: HealthCareModel[MyIO] = new HealthCareModel[MyIO]
    val n                            = 100000000
    val seeds                        = (1 to n).map(_ => Random.nextFloat())
    println(s"Running $n times")
    val (finalState, outputs)        =
      seeds.foldLeft((initialState, MyIO(() => println("Started")))) { case (acc, seed) =>
        val (state: HealthCareDemand, output: MyIO[Unit]) = acc
        val (newState, newOutput)                         = model.transition(state, seed)
        (newState, newOutput *> output)  // this *> appears to create a new MyIO
      }
    println(finalState)
    println(s"Total ${finalState.total}")
  }

}

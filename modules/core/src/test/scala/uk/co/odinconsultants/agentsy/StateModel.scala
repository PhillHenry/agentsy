package uk.co.odinconsultants.agentsy

import cats.{Applicative, Monad}
import shapeless.ops.coproduct.FlatMap

trait Model[T[_]] {
  type State
  type Input
  type Output
  type StateTransition = (State, Input) => T[(State, Output)]
}

case class HealthCareDemand(gp: Int, emergency: Int, ambulance: Int)

class HealthCareModel[T[_]: Monad, A] {
  type State = HealthCareDemand
  type Input = Float
  val walkInThreshold      = 0.4f
  val ambulanceThreshold   = 0.5f
  val typicalWalkInSeed    = 0.39f
  val typicalAmbulanceSeed = 0.49f
  val typicalGPSeed        = 0.51f
  def transition(
      incEmergency: T[A],
      incAmbulance: T[A],
      incGP:        T[A],
  ): (HealthCareDemand, Float) => T[(HealthCareDemand, T[A])] =
    (state, input) =>
      Applicative[T].pure {
        if (input < walkInThreshold) // walk-in
          (state.copy(emergency = state.emergency + 1), incEmergency)
        else if (input < ambulanceThreshold)
          (state.copy(ambulance = state.ambulance + 1), incAmbulance)
        else
          (state.copy(ambulance = state.ambulance + 1), incGP)
      }
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

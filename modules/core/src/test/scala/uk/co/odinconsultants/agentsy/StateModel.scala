package uk.co.odinconsultants.agentsy

import cats.{Applicative, Monad}

trait Model[T[_]] {
  type State
  type Input
  type Output
  type StateTransition = (State, Input) => T[(State, Output)]
}

case class HealthCareDemand(gp: Int, emergency: Int, ambulance: Int)

class HealthCareModel[T[_]: Monad, A] {
  type State     = HealthCareDemand
  type Input     = Float
  type Output[A] = T[A]
  def initialState(
      incEmergency: Output[A],
      incAmbulance: Output[A],
      incGP:        Output[A],
  ): (HealthCareDemand, Float) => T[(HealthCareDemand, Output[A])] =
    (state, input) =>
      Applicative[T].pure {
        if (input < 0.4) // walk-in
          (state.copy(emergency = state.emergency + 1), incEmergency)
        else if (input < 0.5)
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

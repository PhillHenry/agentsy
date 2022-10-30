package uk.co.odinconsultants.agentsy

import cats.Applicative

trait Model[T[_]: Applicative] {
  type State
  type Input
  type Output
  type StateTransition = (State, Input) => T[(State, Output)]
}

case class HealthCareDemand(gp: Int, emergency: Int, ambulance: Int)

class HealthCareModel[T[_]: Applicative] extends Model[T] {
  type State  = HealthCareDemand
  type Input  = Int
  type Output = Unit
  val initialState: StateTransition = (state, input) =>
    Applicative[T].pure {
      ???
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

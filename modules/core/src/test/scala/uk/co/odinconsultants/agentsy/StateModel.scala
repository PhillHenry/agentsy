package uk.co.odinconsultants.agentsy

import cats.Applicative

class StateModel[T[_]: Applicative] {
  type State = Long
  type Input = Int
  type Output = Unit
  type StateTransition = (State, Input) => T[(State, Output)]
  val run: StateTransition = (state, input) =>
    Applicative[T].pure( {
      val newState: Long = state + input
      (newState, println(s"$state - $input -> $newState"))
    })
}

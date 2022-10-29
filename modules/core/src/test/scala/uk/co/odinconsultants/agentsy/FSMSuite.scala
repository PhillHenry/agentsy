package uk.co.odinconsultants.agentsy
import weaver.SimpleIOSuite
import weaver.scalacheck.Checkers
import cats.effect.IO
import cats.effect.*

object FSMSuite  extends SimpleIOSuite with Checkers {
  test("state transition") {
    type State = Long
    type Input = Int
    type Output = Unit
    type StateTransition = (State, Input) => IO[(State, Output)]
    val run: StateTransition = (state, input) => IO {
      val newState: Long = state + input
      (newState, println(s"$state - $input -> $newState"))
    }
    val intialState = FSM(run)
    for {
      (state, _)  <- intialState.run(100, 1)
    } yield {
      expect.same(state, 101)
    }
  }
}

package uk.co.odinconsultants.agentsy
import weaver.SimpleIOSuite
import weaver.scalacheck.Checkers
import cats.effect.IO

object FSMSuite  extends SimpleIOSuite with Checkers {
  test("state transition") {
    val model = new StateModel[IO]
    val intialState = FSM(model.run)
    for {
      (state, _)  <- intialState.run(100, 1)
    } yield {
      expect.same(state, 101)
    }
  }
}

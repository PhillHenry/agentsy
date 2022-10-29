package uk.co.odinconsultants.agentsy
import weaver.SimpleIOSuite
import weaver.scalacheck.Checkers
import cats.effect.IO

object FSMIOSuite extends SimpleIOSuite with Checkers {
  test("state transition") {
    val model = new StateModel[IO]
    val initialState = FSM(model.run)
    for {
      (state, _)  <- initialState.run(100, 1)
    } yield {
      expect.same(state, 101)
    }
  }
}

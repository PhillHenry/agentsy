package uk.co.odinconsultants.agentsy

import org.scalatest.*
import org.scalatest.matchers.Matcher
import cats.Id

class FSMSpec extends wordspec.AnyWordSpec {

  "FSM" should {
    "work in a single thread" in {
      val model = new StateModel[Id]
      val initialState = FSM(model.run)
      assert(initialState.run(100, 1)._1 == 101)
    }
  }

}

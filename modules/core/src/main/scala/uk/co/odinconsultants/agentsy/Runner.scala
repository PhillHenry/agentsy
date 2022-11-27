package uk.co.odinconsultants.agentsy

import fs2.Stream
import Effects.*
import scala.util.Random.nextFloat

object Runner {

  def main(args: Array[String]) : Unit = {
    val randoms = Stream.eval(StateEffect(() => nextFloat())).repeat

    /**
     * Cannot find an implicit Compiler[F, G]. This typically means you need a Concurrent[F] in
     * scope.
     */
//    println(randoms.take(10).compile.toList)
  }

}

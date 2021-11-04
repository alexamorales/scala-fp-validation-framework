package com.validation

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import zio.ZIO

class ValidatorTest extends AnyWordSpec with Matchers with TestRuntime {

  val error: Validator.Error[Unit] = Validator.Error
    .make((), ExecutionStatus.failed("1.0", "Test", "1.0", "Error"))

  "Validator" when {
    "create" should {
      "fail fast" in {
        val checks = List[Check[Int]](
          _ => Option.empty,
          _ => Option(error.status),
          _ => Option(error.status)
        )
        val input = (_: Int) => ZIO.succeed(0)
        val validator =
          Validator.make[Int, Int, Unit, Unit](checks)(input, _ => ((), ()))
        unsafeRun(validator.apply(1).either) mustBe Left(error)
      }
    }
  }

}

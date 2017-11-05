package io.polynect.scala.validation

import org.scalatest._


class ValidatedOnceSpec extends FlatSpec with Matchers {

  var state = 0

  case class NotPure() extends ValidatedOnce {
    def rules: Errors = {
      validateRef
    }
    def validateRef: Errors = {
      println("validating")
      if (state != 0) err("state", "State should be 0") else Nil
    }
  }

  "Validation" should "cache result" in {
    val np = NotPure()
    np.isValid shouldEqual true
    state = 1
    np.isValid shouldEqual true
  }

  "Validation" should "cache error result" in {
    val np = NotPure()
    state = 1
    np.isValid shouldEqual false
    state = 0
    np.isValid shouldEqual false
  }

}

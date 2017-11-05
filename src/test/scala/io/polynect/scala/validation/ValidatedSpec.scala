package io.polynect.scala.validation

import org.scalatest._

class ValidatedSpec extends FlatSpec with Matchers {

  case class Person(name: String, age: Int, mother: Option[Person] = None) extends Validated {
    def rules: Errors = combine(
      gt("age", 0)(age),
      notEmpty[String]("name", name, _.isEmpty),
      ifDefined[Person](mother, m =>
        gt("mother.age", this.age)(m.age)),
      embedded("mother", mother)
    )
  }

  "A validated" should "produce errors on invalid input" in {
    val p = Person("", -2)
    p.isValid shouldEqual false
    p.getErrors.length shouldEqual  2
  }

  "An option" should "be validated if defined" in {
    val p = Person("child", 20, Some(Person("mother", 15)))
    p.isValid shouldEqual false
    p.getErrors.exists(_.field == "mother.age") shouldEqual true
  }

}

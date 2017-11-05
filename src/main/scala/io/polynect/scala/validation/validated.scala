package io.polynect.scala.validation


case class ValidationError(field: String, msg: String)

trait Validated {
  type Errors = Seq[ValidationError]

  protected def err(field: String, msg: String) = List(ValidationError(field, msg))

  protected def rules: Errors

  def getErrors: Errors = rules

  def gt[T](field: String, value: T)(actual: T)(implicit ord: Ordering[T]): Errors = {
    if (!ord.gt(actual, value)) err(field, "Must be greater than " + value) else Nil
  }

  def lt[T](field: String, value: T)(actual: T)(implicit ord: Ordering[T]): Errors = {
    if (!ord.lt(actual, value)) err(field, "Must be less than " + value) else Nil
  }

  def gte[T](field: String, value: T)(actual: T)(implicit ord: Ordering[T]): Errors = {
    if (!ord.gteq(actual, value)) err(field, "Must be greater than or equal to" + value) else Nil
  }

  def lte[T](field: String, value: T)(actual: T)(implicit ord: Ordering[T]): Errors = {
    if (!ord.lteq(actual, value)) err(field, "Must be less than or equal to " + value) else Nil
  }

  def inSet[T](field: String, set: Set[T])(value: T): Errors =
    if (!set.contains(value)) err(field, s"Must be one of " + set.mkString(", ")) else Nil

  def notInSet[T](field: String, set: Set[T])(value: T): Errors =
    if (set.contains(value)) err(field, s"Must not be one of " + set.mkString(", ")) else Nil

  def notEmpty[T](field: String, value: T, isEmpty: T => Boolean) =
    if (isEmpty(value)) err(field, "Must not be empty") else Nil

  def notEmptyString(field: String)(actual: String): Errors = {
    if (actual.isEmpty) err(field, "Must not be empty") else Nil
  }
  def combine(errors: Errors*): Errors = errors.flatten

  def embedded(field: String, obj: Validated): Errors =
    obj.getErrors.map(error =>
      ValidationError(s"$field.${error.field}", error.msg)
    )

  def embedded(field: String, obj: Option[Validated]): Errors =
    obj.map(embedded(field, _)).getOrElse(Nil)

  def isValid = getErrors.isEmpty

  def ifDefined[T <: Validated](obj: Option[T], f: T => Errors) = obj.map(f).getOrElse(Nil)
}

trait ValidatedOnce extends Validated {
  private var errors: Errors = Nil
  private var cached: Boolean = false
  override def getErrors: Errors = {
    if (!cached) {
      errors = super.getErrors
      cached = true
    }
    errors
  }
}
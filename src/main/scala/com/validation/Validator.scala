package com.validation

import zio.IO
import zio._

object Validator {

  implicit class Ops[I, C, O](val validator: Validator[I, C, O]) extends AnyVal {

    def mapError(f: ExecutionStatus => ExecutionStatus): Validator[I, C, O] =
      Validator.mapError(validator, f)
  }

  def mergeTakeRightOutput[I, C, O](v1: Validator[I, C, _], v2: Validator[I, C, O]): Validator[I, C, O] =
    input => v1(input) *> v2(input)

  def mergeTakeLeftOutput[I, C, O](v1: Validator[I, C, O], v2: Validator[I, C, _]): Validator[I, C, O] =
    input => v1(input) <* v2(input)

  def mergeIgnoreOutput[I, C](v1: Validator[I, C, _], v2: Validator[I, C, _]): Validator[I, C, Unit] =
    input => (v1(input) *> v2(input)).unit

  def mapError[I, C, O](v: Validator[I, C, O], f: ExecutionStatus => ExecutionStatus): Validator[I, C, O] =
    input => v(input).mapError(e => e.copy(status = f(e.status)))

  def chain[I, C, O1, O2](v1: Validator[I, C, O1], v2: Validator[O1, C, O2]): Validator[I, C, O2] =
    input => v1(input).flatMap(output => v2(output))

  def succeed[I, C, O](output: => O): Validator[I, C, O] = _ => IO.succeed(output)

  def succeed_[I, C]: Validator[I, C, Unit] = _ => IO.succeed(())

  def fail[I, C, O](e: Error[C]): Validator[I, C, O] = _ => IO.fail(e)

  def fail[I, C, O](status: ExecutionStatus): Validator[I, C, O] =
    _ => IO.fail(Error(context = Option.empty[C], status))

  def fromEither[I, C, O](either: Either[Error[C], O]): Validator[I, C, O] =
    either match {
      case Right(output) => Validator.succeed(output)
      case Left(error)   => Validator.fail(error)
    }

  def fromEitherStatus[I, C, O](either: Either[ExecutionStatus, O]): Validator[I, C, O] = {
    import cats.syntax.either._
    val e = either.leftMap(status => Error(context = Option.empty[C], status))
    Validator.fromEither(e)
  }

  case class Error[C](context: Option[C], status: ExecutionStatus, acceptableValidityPeriod: Option[Int] = Option.empty)

  object Error {
    def make[C](status: ExecutionStatus): Error[C]             = Error[C](Option.empty[C], status)
    def make[C](context: C, status: ExecutionStatus): Error[C] = Error[C](Option(context), status)
    def make[C](context: C, status: ExecutionStatus, acceptableValidityPeriod: Int): Error[C] =
      Error[C](Option(context), status, Option(acceptableValidityPeriod))
  }

  /**
   * Creates a validator that takes I1 as input and return either an output O or an Error with a context C
   *
   * @param cs List of validation checks: Input => Option[ExecutionStatus]
   * @param i Function that transform pure input I1 into extended one I2 (it can contain extra data from DB)
   * @param o Function that transform extended input into context C and output O
   * @tparam I1 Pure input
   * @tparam I2 Extended input e.g. Request + required data from DB
   * @tparam C Context type used in Error[C]
   * @tparam O Output type e.g. some updated intermediate data
   * @return Validation output
   */
  def make[I1, I2, C, O](cs: List[Check[I2]])(i: I1 => IO[ExecutionStatus, I2], o: I2 => (C, O)): Validator[I1, C, O] =
    i(_).either.flatMap {
      case Right(input) => runChecks(input, cs).mapBoth(status => Error.make(o(input)._1, status), _ => o(input)._2)
      case Left(status) => IO.fail(Error.make(status))
    }

  def makeC[I1, I2, C](cs: List[Check[I2]])(i: I1 => IO[ExecutionStatus, I2], o: I2 => C): Validator[I1, C, C] =
    make(cs)(i, o.andThen(c => (c, c)))

  def makeM[I1, I2, C, O](
    cs: List[Check[I2]]
  )(i: I1 => IO[ExecutionStatus, I2], o: I2 => IO[ExecutionStatus, O], e: I2 => C): Validator[I1, C, O] =
    i(_).either.flatMap {
      case Right(input) => runChecks(input, cs).flatMap(_ => o(input)).mapError(s => Error.make(e(input), s))
      case Left(status) => IO.fail(Error.make(status))
    }

  def makeM_[I1, I2, C, O](
    cs: List[Check[I2]]
  )(i: I1 => IO[ExecutionStatus, I2], o: I2 => IO[ExecutionStatus, O]): Validator[I1, C, O] =
    i(_).either.flatMap {
      case Right(input) => runChecks(input, cs).flatMap(_ => o(input)).mapError(s => Error.make(s))
      case Left(status) => IO.fail(Error.make(status))
    }

  def make_[I1, I2, C](cs: List[Check[I2]])(i: I1 => IO[ExecutionStatus, I2], e: I2 => C): Validator[I1, C, Unit] =
    make(cs)(i, i => (e(i), ()))

  def make__[I1, I2, C](cs: List[Check[I2]])(i: I1 => IO[ExecutionStatus, I2]): Validator[I1, C, Unit] =
    i(_).either.flatMap {
      case Right(input) => runChecks(input, cs).mapBoth(status => Error.make(status), _ => ())
      case Left(status) => IO.fail(Error.make(status))
    }

  @annotation.tailrec
  private def runChecks[I](input: I, checks: List[Check[I]]): IO[ExecutionStatus, Unit] =
    checks match {
      case h :: t =>
        h(input) match {
          case Some(status) => IO.fail(status)
          case None         => runChecks(input, t)
        }
      case _ => IO.unit
    }
}

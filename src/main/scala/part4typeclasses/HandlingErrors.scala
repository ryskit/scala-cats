package part4typeclasses

import cats.{Applicative, ApplicativeError, Monad}

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

object HandlingErrors {

  trait MyApplicativeError[M[_], E] extends Applicative[M] {
    def raiseError[A](e: E): M[A]
    def handleErrorWith[A](ma: M[A])(f: E => M[A]): M[A]
    def handleError[A](ma: M[A])(f: E => A) = handleErrorWith(ma)(e => pure(f(e)))
  }

  trait MyMonadError[M[_], E] extends ApplicativeError[M, E] with Monad[M] {
    def ensure[A](ma: M[A])(error: E)(predicate: A => Boolean): M[A]
  }

  import cats.MonadError
  import cats.instances.either._

  type ErrorOr[A] = Either[String, A]
  val monadErrorEither = MonadError[ErrorOr, String]
  val success = monadErrorEither.pure(32) // Either[String, Int] == Right(32)
  val failure = monadErrorEither.raiseError[Int]("something wrong") // Either[String, Int] == Left("something wrong")
  // recover
  val handleError: Either[String, Int] = monadErrorEither.handleError(failure) {
    case "Badness" => 44
    case _ => 89
  }
  // recoverWith
  val handledError2: ErrorOr[Int] = monadErrorEither.handleErrorWith(failure) {
    case "Badness" => monadErrorEither.pure(44) // ErrorOr[Int]
    case _ => Left("something else") // ErrorOr[Int]
  }
  // "filter"
  val filteredSuccess = monadErrorEither.ensure(success)("Number too small")(_ > 100)

  // Try and Future
  import cats.instances.try_._
  val exception = new RuntimeException("Ready bad")
  val pureException = MonadError[Try, Throwable].raiseError(exception) // Failure(exception)
  import cats.instances.future._
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  MonadError[Future, Throwable].raiseError(exception) // Future which will complete with a Failure(exception)

  // applicatives => ApplicativeError
  import cats.data.Validated
  import cats.instances.list._
  type ErrorsOr[T] = Validated[List[String], T]
  import cats.ApplicativeError
  val applicativeErrorVal = ApplicativeError[ErrorsOr, List[String]]

  import cats.syntax.applicative._
  import cats.syntax.applicativeError._
  val extendedSuccess = 42.pure
  val extendedError = List("Badness").raiseError[ErrorsOr, Int]
  val recoveredError: ErrorsOr[Int] = extendedError.recover {
    case _ => 43
  }

  import cats.syntax.monadError._
  val testedSuccess = success.ensure("something bad")(_ > 100)

  def main(args: Array[String]): Unit = {

  }
}

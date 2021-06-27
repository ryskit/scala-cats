package part4typeclasses

object Applicatives {

  // Applicatives = Functors + the pure method
  import cats.Applicative
  import cats.instances.list._
  val listApplicative = Applicative[List]
  val aList = listApplicative.pure(2) // List(2)

  import cats.instances.option._
  val optionApplicative = Applicative[Option]
  val anOption = optionApplicative.pure(2) // Some(2)

  // pure extension method
  import cats.syntax.applicative._
  val aSweetList = 2.pure[List] // List(2)
  val aSweetOption = 2.pure[Option] // Some(2)

  // Monads extend Applicative
  // Applicatives extend Functors
  import cats.data.Validated
  type ErrorsOr[T] = Validated[List[String], T]
  val aValidValue: ErrorsOr[Int] = Validated.valid(43) // "pure"
  val aModifiedValidated: ErrorsOr[Int] = aValidValue.map(_ + 1) // map
  val validatedApplicative = Applicative[ErrorsOr]

  // TODO: thought experiment
  def ap[W[_], A, B](wf: W[A => B])(wa: W[A]): W[B] = ???
  def productWithApplicative[W[_], A, B](wa: W[A], wb: W[B])(
    implicit applicative: Applicative[W]
  ): W[(A, B)] = {
    val functionWrapper: W[B => (A, B)] =
      applicative.map(wa)(a => (b: B) => (a, b))
    ap(functionWrapper)(wb)
  }

  // Applicatives have this ap[W[_], A, B](wf: W[A => B])(wa: W[A]): W[B]
  // Applicatives can implement product from Semigroupal
  // => Applicatives extends Semigroupal
  def main(args: Array[String]): Unit = {}
}

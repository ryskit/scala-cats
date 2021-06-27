package part4typeclasses

import cats.Functor
import cats.Semigroupal

object WeakerApplicatives {

  trait MyApply[W[_]] extends Functor[W] with Semigroupal[W] {
    override def product[A, B](fa: W[A], fb: W[B]): W[(A, B)] = {
      val functorWrapper: W[B => (A, B)] = map(fa)(a => (b: B) => (a, b))
      ap(functorWrapper)(fb)
    }

    // TODO
    def mapN[A, B, C](tuple: (W[A], W[B], W[C]))

    def ap[B, T](wf: W[B => T])(wb: W[B]): W[T] = ???
  }

  import cats.Apply
  import cats.instances.option._
  val applyOption = Apply[Option]
  val funcApp = applyOption.ap(Some((x: Int) => x + 1))(Some(2)) // Some(3)

  import cats.syntax.apply._
  val tupleOfOption = (Option(1), Option(2), Option(3))
  val optionOfTuple = tupleOfOption.tupled // Some((1, 2, 3))
  val sumOption = tupleOfOption.mapN(_ + _ + _) // Some(6)

  trait MyApplicative[W[_]] extends Functor[W] with Semigroupal[W] {
    def pure[A](x: A): W[A] // fundamental

  }

}

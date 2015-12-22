import scalaz._
import Scalaz._

/*
  We can lift our functions (Function1) to become an Arrow
*/
def safeDivOpt(t: (Double, Double)): Option[Double] =
  t match {
    case (_, d) if d==0  => None
    case (n, d) if d!=0 => Some(n/d)
  }
val safeDiv = x => safeDivOpt(x)
val kSafeDiv = Kleisli.kleisli(safeDiv)


/*
  The advantage of working with Kleisli arrows is that we can combine 'em through monad composition
 */

// first let's define a new function
import math.{sqrt}
val safeSqrtOpt = (x: Double) =>
  x match {
    case v if v > 0 => Some(sqrt(v))
    case _ => None
  }
val kSafeSqrt = Kleisli.kleisli(safeSqrtOpt)

// we can then work with the combinators
val pipe = kSafeDiv >>> kSafeSqrt
val sugaredPipe = Kleisli(safeDiv) >==> safeSqrtOpt

val r1 = pipe.run( (10.0,5.0) )
val r2 = pipe.run( (-10.0,5.0) )
val r1bis = sugaredPipe.run( (10.0,5.0) )


/*
  -- Look through the pipeline --
  All this is cool, however we don't know what made the pipeline fail.
  It'd be cool to keep a ledger of what is going on within the pipeline

 */
abstract class MyError
case class NumericalError(msg: String) extends MyError

// Either is a Monad with two type parameters: M[A,B] which represent left and right respectively
// Let's create an ad-hoc type
type EEither[+T] = \/[MyError, T]

// we can define new functions, which we will lift as a Kleisli Arrows, which return \/
def safeDivEither(t: (Double, Double)): EEither[Double] =
  safeDivOpt(t) match {
    case Some(r) => r.right
    case None => NumericalError("Can't divide by zero").left
  }
val kSafeDivEither = Kleisli.kleisli( (x: (Double, Double) ) => safeDivEither(x) )

def safeSqrtEither(t: Double): EEither[Double] =
  safeSqrtOpt(t) match {
    case Some(r) => r.right
    case None => NumericalError("Sqrt on double is not define if _ < 0").left
  }
val kSafeSqrtEither = Kleisli.kleisli( (x: Double) => safeSqrtEither(x) )

val pipeEither = kSafeDivEither >>> kSafeSqrtEither

val r3 = pipeEither.run( (10.0,5.0) )
val r4 = pipeEither.run( (-10.0,5.0) )
val r5 = pipeEither.run( (0.0,5.0) )


val pipeEither2 = kSafeSqrtEither >>> kSafeSqrtEither
val r5b = pipeEither2 run 16.0

/*
  This is great. How to join two pipelines?
  Work with tuples
 */
val tupledPipe = (pipeEither *** pipeEither) >>> kSafeDivEither
val r6 = tupledPipe run ((100.0, 4.0), (25.0, 1.0))


/*
  Wow, this is pretty sleek.
  However, we can not keep a ledger of what happens during the pipeline
  -- Introducing the Writer monad --
*/

def minusOneEither(x: Double): EEither[Double] = (x - 1.0).right
val kMinusOne = Kleisli.kleisli( (x: Double)  =>  minusOneEither(x) )
val weirdPipeEither =
  (kSafeDivEither *** (kSafeDivEither >>> kMinusOne)) >>> kSafeDivEither
val failsButWhere = weirdPipeEither run ((4.0,1.0), (1.0,1.0))

type Wrtr[O] = WriterT[EEither, NonEmptyList[String],O]

val sqrtWithLog: Kleisli[Wrtr, Double, Double] =
  Kleisli.kleisli[Wrtr, Double, Double](t =>
    WriterT.put(safeSqrtEither(t))(s"squared $t".wrapNel)
  )

val safeDivWithLog: Kleisli[Wrtr, (Double,Double), Double] =
  Kleisli.kleisli[Wrtr, (Double, Double), Double]( (t: (Double, Double)) => {
    val (n,d) = t
    WriterT.put(safeDivEither(t))(s"divided $n by $d".wrapNel)
  })

val combinedFunction = sqrtWithLog >>> sqrtWithLog
val r = combinedFunction run 16.0

val combinedFunction2 = safeDivWithLog >>> sqrtWithLog
val rAgain = combinedFunction2 run (-10.0,2.0)
val w = rAgain.written


/*
  OK, all this makes sense.
  However, we have dropped the log file. How's that?
  Well, it turns out monads are not commutative
*/

type W[A] = Writer[List[String], A]
type E[A] = EitherT[W, MyError, A]

val sqrtWithLog2: Kleisli[E, Double, Double] =
  Kleisli.kleisli[E, Double, Double](t =>
    EitherT[W, MyError, Double](safeSqrtEither(t).set(List(s"squared $t")))
  )

val constNegative1: Kleisli[E, Double, Double] =
  Kleisli.kleisli[E, Double, Double](_ => -1.0.point[E])

val combinedFunctionEitherT = sqrtWithLog2 >>> constNegative1 >>> sqrtWithLog2
combinedFunctionEitherT.run(16.0).run.written
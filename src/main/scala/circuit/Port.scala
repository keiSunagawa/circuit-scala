package circuit

import shapeless._

sealed trait PState
object PState {
  sealed trait NCn extends PState
  case object NCn extends NCn
  sealed trait Cn extends PState
  case object Cn extends Cn
}

trait AtC[L <: HList, N <: Nat] {
  type CnL <: HList
  def cn(l: L): CnL
}

object AtC {
  import PState._

  type Aux[L<: HList, N <: Nat, CnL0 <: HList] = AtC[L, N] { type CnL = CnL0 }

  implicit def hlistAtCZero[T <: HList]: Aux[NCn :: T, _0, Cn :: T] =
    new AtC[NCn :: T, _0] {
      type CnL = Cn :: T

      def cn(l: NCn :: T): CnL = {
        val _ :: t = l
        Cn :: t
      }
    }

  implicit def hlistAtCn[H, T <: HList, N <: Nat]
    (implicit at : AtC[T, N]): Aux[H :: T, Succ[N], H :: at.CnL] =
    new AtC[H :: T, Succ[N]] {
      type CnL = H :: at.CnL
      def cn(l : H :: T) : CnL = {
        val h :: t = l
        h :: at.cn(t)
      }
    }
}

sealed trait Circuit[A]
object Circuit {
  import PState._
  type Port2[P1 <: PState, P2 <: PState] = P1 :: P2 :: HNil
  type Port3[P1 <: PState, P2 <: PState, P3 <: PState] = P1 :: P2 :: P3 :: HNil
  case class NewWire() extends Circuit[Port2[NCn, NCn]]
  case class NewForkWire() extends Circuit[Port3[NCn, NCn, NCn]]
  case class NewNAND() extends Circuit[Port3[NCn, NCn, NCn]]
  case class Connect[A <: HList, B <: HList, C <: HList, D <: HList](a: A, b: B, an: Int, bn: Int) extends Circuit[(C, D)]
}

object DSL {
  import Circuit._
  import PState._
  import shapeless.ops.nat.ToInt
  import cats.free.Free
  import cats.free.Free.liftF

  type DSL[A] = Free[Circuit, A]
  def newWire: DSL[Port2[NCn, NCn]] = liftF(NewWire())
  def newForkWire: DSL[Port3[NCn, NCn, NCn]] = liftF(NewForkWire())
  def newNAND: DSL[Port3[NCn, NCn, NCn]] = liftF(NewNAND())
  def connect[A <: HList, B <: HList](a: A, b: B, an: Nat, bn: Nat)(implicit atc1: AtC[A, an.N], atc2: AtC[B, bn.N], a2i: ToInt[an.N], b2i: ToInt[bn.N]): DSL[(atc1.CnL, atc2.CnL)] = {
    val x: Circuit[(atc1.CnL, atc2.CnL)] = Connect[A, B, atc1.CnL, atc2.CnL](a, b, Nat.toInt(an), Nat.toInt(bn))
    liftF(x)
  }

  val NOT = for {
    w1 <- newForkWire
    nand <- newNAND
    cn <- connect(w1, nand, 1, 1)
    cn2 <- connect(cn._1, cn._2, 2, 2)
  } yield cn2
}

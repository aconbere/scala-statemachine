import scala.io.Source

import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

import org.junit.runner.RunWith

import com.conbere.statemachine._

case object Locked extends State
case object UnLocked extends State

case class Coin(val value: Int) extends Event
case object Push extends Event

class Turnstile(val value: Int, val state: State)
extends StateMachine[Turnstile] {
  def this() = this(0, Locked)

  val requiredPayment = 10

  override def defaultTransition: Transition = {
    case _ => this
  }

  val transitions: Transition = {
    case (Locked, Coin(v)) =>
      if (value + v >= requiredPayment)
        new Turnstile(0, UnLocked)
      else
        new Turnstile(value + v, state)
    case (Locked, Push) =>
      this
    case (UnLocked, Push) =>
      new Turnstile()
    case (UnLocked, Coin(_)) =>
      this
  }

  override def toString = "Turnstile: %s: %s".format(state, value)
}

@RunWith(classOf[JUnitRunner])
class StateMachineSuite extends FunSuite {
  trait StateMachineM {
    val t = new Turnstile()
  }

  test("turnstile") {
    new StateMachineM {
      assert(t.state == Locked)
      assert(t.value == 0)

      val t2 = t.trigger(Push)

      assert(t2.state == Locked)
      assert(t2.value == 0)

      val t3 = t2.trigger(Coin(5))

      assert(t3.state == Locked)
      assert(t3.value == 5)

      val t4 = t3.trigger(Coin(5))

      assert(t4.state == UnLocked)
      assert(t4.value == 0)

      val t5 = t4.trigger(Push)

      assert(t5.state == Locked)
      assert(t5.value == 0)
    }
  }
}

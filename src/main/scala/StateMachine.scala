package com.conbere.statemachine

trait Event
trait State

trait StateMachine[Self <: StateMachine[Self]] {
  type Transition = PartialFunction[(State, Event), Self]

  val transitions: Transition
  val state: State

  def defaultTransition: Transition

  def trigger(event: Event): Self = {
    (transitions orElse defaultTransition)((state, event))
  }
}


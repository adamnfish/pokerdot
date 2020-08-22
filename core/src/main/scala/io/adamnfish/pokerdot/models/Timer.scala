package io.adamnfish.pokerdot.models


/**
 * Represents a poker timer in progress.
 *
 * timerStartTime is a UTC timestamp used to determine progress through the timer phases.
 * It will be adjusted by pausing / playing.
 *
 * If the timer is paused we need to know *when* so we can still calculate the current phase.
 */
case class TimerStatus(
  timerStartTime: Long,
  pausedTime: Option[Long],
  levels: List[TimerLevel],
)

sealed trait TimerLevel
case class RoundPhase(
  smallBlind: Int,
  durationSeconds: Int,
) extends TimerLevel
case class Break(
  durationSeconds: Int,
) extends TimerLevel

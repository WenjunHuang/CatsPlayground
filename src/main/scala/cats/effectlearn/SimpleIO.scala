package cats.effectlearn

import java.util.concurrent.ScheduledExecutorService

import cats.effect.IO

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

object SimpleIO extends App {
  def delayedTick(d: FiniteDuration)(
      implicit sc: ScheduledExecutorService): IO[Unit] =
    IO.cancelable { cb =>
      val r = new Runnable { def run() = cb(Right(())) }
      val f = sc.schedule(r, d.length, d.unit)

      IO(f.cancel(false))

    }

  val timer = IO.timer(ExecutionContext.global)
}

package cats.effectlearn

import java.io._

import cats.effect._
import cats.effect.concurrent.Semaphore
import cats.implicits._

object FileCopier extends IOApp {

  override def run(args: List[String]): IO[ExitCode] =
    for {
      _ <- checkArguments(args)
      orig = new File(args.head)
      dest = new File(args(1))
      _ <- checkOriginalTargetIsTheSame(orig, dest)
      w <- overrideTargetIfExist(dest)
      _ <- if (w) {
            for {
              count <- copy[IO](orig, dest, 100)
              _ <- IO(println(
                    s"$count bytes copied from ${orig.getPath} to ${dest.getPath}"))
            } yield ()
          } else {
            IO.unit
          }
    } yield ExitCode.Success

  private def overrideTargetIfExist(dest: File) =
    if (dest.exists()) {
      for {
        _ <- IO(println(s"do you want to override existing destination file?"))
        input <- IO(scala.io.StdIn.readLine)
      } yield {
        if (input.toLowerCase() == "y")
          true
        else
          false
      }
    } else IO.pure(true)

  private def checkOriginalTargetIsTheSame(orig: File,
                                           dest: File) =
    if (orig.compareTo(dest) == 0)
      IO.raiseError(
        new IllegalArgumentException(
          "Origin and destination files can not be the same"))
    else IO.unit

  private def checkArguments(args: List[String]): IO[Unit] =
    if (args.length < 2)
      IO.raiseError(
        new IllegalArgumentException("Need origin and destination files"))
    else IO.unit

  def inputStream[F[_]: Sync](
      f: File,
      guard: Semaphore[F]): Resource[F, FileInputStream] =
    Resource.make {
      Sync[F].delay(new FileInputStream(f))
    } { inStream =>
      guard.withPermit {
        Sync[F].delay(inStream.close()).handleErrorWith(_ => Sync[F].unit)
      }
    }

  def outputStream[F[_]: Sync](
      f: File,
      guard: Semaphore[F]): Resource[F, FileOutputStream] =
    Resource.make {
      Sync[F].delay(new FileOutputStream(f))
    } { outStream =>
      guard.withPermit {
        Sync[F].delay(outStream.close()).handleErrorWith(_ => Sync[F].unit)
      }
    }

  def inputOutputStreams[F[_]: Sync](
      in: File,
      out: File,
      guard: Semaphore[F]): Resource[F, (InputStream, OutputStream)] =
    for {
      inStream <- inputStream(in, guard)
      outStream <- outputStream(out, guard)
    } yield (inStream, outStream)

  def transmit[F[_]: Sync](origin: InputStream,
                           destination: OutputStream,
                           buffer: Array[Byte],
                           acc: Long): F[Long] =
    for {
      amount <- Sync[F].delay(origin.read(buffer, 0, buffer.length))
      count <- if (amount > -1)
                Sync[F].delay(destination.write(buffer, 0, amount)) >> transmit(
                  origin,
                  destination,
                  buffer,
                  acc + amount)
              else Sync[F].pure(acc)
    } yield count

  def transfer[F[_]: Sync](origin: InputStream,
                           destination: OutputStream,
                           bufferSize: Int): F[Long] =
    for {
      buffer <- Sync[F].delay(new Array[Byte](bufferSize))
      total <- transmit(origin, destination, buffer, 0L)
    } yield total

  def copy[F[_]: Concurrent](origin: File,
                             destination: File,
                             bufferSize: Int): F[Long] =
    for {
      guard <- Semaphore[F](1)
      count <- inputOutputStreams(origin, destination, guard).use {
                case (in, out) =>
                  transfer(in, out, bufferSize)
              }
    } yield count

}

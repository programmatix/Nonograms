package nonograms.ui

import java.util.Date

import org.scalajs.dom.html.Div

import scala.collection.mutable.ArrayBuffer
import scala.scalajs.js.timers.setTimeout
import scala.util.Random
import scalatags.JsDom.all.{cls, div, _}

case class ParticleSettings(gravity: Float = 1.0f,
                            updatesPerSec: Float = 20.0f,
                            numParticlesOnSuccess: Int = 20,
                            numParticlesOnFailure: Int = 20,
                            maxVelocityX: Float = 5.0f,
                            maxVelocityY: Float = 7.0f,
                            xJigglePixels: Int = 10,
                            yJigglePixels: Int = 10,
                            minLifetimeMsecs: Int = 300,
                            cleanupEveryXMecs: Int = 500,
                            animate: Int = 1,
                            updateDelta: Float = 0.05f)

case class AnimationParticleBurst(clsName: String, top: Div, xPixels: Float, yPixels: Float, ps: ParticleSettings, onFinished: () => Unit) extends Animation {
//  square.rendered.className = square.rendered.className + " square-success-anim"
  private val particles = ArrayBuffer.empty[Particle]
  private var i = ps.numParticlesOnSuccess
  private val started = new Date().getTime()
//  private val lifetimeRange = ps.maxLifetimeMsecs - ps.minLifetimeMsecs

  while (i > 0) {
    val xVelocity = ((ps.maxVelocityX * (Random.nextInt(1000).toFloat) / 1000.0f) * 2) - ps.maxVelocityX
//    val yVelocity = -1 * (((ps.maxVelocityY * (Random.nextInt(1000).toFloat) / 1000.0f) * 2) - ps.maxVelocityY)
    val yVelocity = -1 * (ps.maxVelocityY * (Random.nextInt(1000).toFloat) / 1000.0f)

//    val lifetimeMsecs = Random.nextInt(lifetimeRange) + ps.minLifetimeMsecs
    val lifetimeMsecs = ps.minLifetimeMsecs

//    println(s"Creating particle x=${xPixels} y=${yPixels} xvel=${xVelocity} yvel=${yVelocity} lifetime=${lifetimeMsecs}")

    val xJiggle = (ps.xJigglePixels * (Random.nextInt(1000).toFloat / 1000.0f) * 2) - ps.xJigglePixels
    val yJiggle = (ps.yJigglePixels * (Random.nextInt(1000).toFloat / 1000.0f) * 2) - ps.yJigglePixels
    val x = xPixels + xJiggle
    val y = yPixels + yJiggle

    val particle = Particle(clsName, top, started, x, y, xVelocity, yVelocity, lifetimeMsecs, ps)
    particles += particle
    i -= 1
  }

  var done = false

  setTimeout(ps.minLifetimeMsecs) {
    done = true
//    onFinished()
  }
}

case class Particle(clsName: String, childOf: Div, startedTime: Long, initialX: Float, initialY: Float, xVelocity: Float, yVelocityInitial: Float, lifeTimeMsecs: Float, settings: ParticleSettings) {
  val rendered: Div = div(cls := "particle " + clsName)().render
  childOf.appendChild(rendered)

  private var yVelocity = yVelocityInitial
  private var x = initialX
  private var y = initialY
  private var isDead = false

  private val updateEveryXMsecs = 1000.0f / settings.updatesPerSec
  private val updateDelta = updateEveryXMsecs * settings.updateDelta

  update()

  def update(): Unit = {
    if (!isDead) {
//      println(s"yvel=${yVelocity} $x $y")

      // y is 0 at the top of the particle board.  A positive Y is going downwards.
      x += xVelocity * updateDelta
      y += yVelocity * updateDelta

      yVelocity += settings.gravity * updateDelta


      rendered.style.top = y.toString + "px"
      rendered.style.left = x.toString + "px"

      val lifetime = (new Date().getTime - startedTime)

      if (lifetime > lifeTimeMsecs) {
        rendered.className += " particle-dead"
        isDead = true
      }

      if (settings.animate == 1) {
        setTimeout(updateEveryXMsecs) {
          update()
        }
      }
    }
  }
}

class Particles(val squareSizePixels: Int, boardWidthSquares: Int, boardHeightSquares: Int) {
  private val animations = ArrayBuffer.empty[Animation]

  // Give ourselves some padding so particles don't clip right off the edge
  private val offsetInSquaresX = 2
  private val offsetInSquaresY = 0

  val rendered = div(cls := "particle-board").render

  rendered.style.top = (-offsetInSquaresY * squareSizePixels).toString + "px"
  rendered.style.left = (-offsetInSquaresX * squareSizePixels).toString + "px"
  rendered.style.width = ((offsetInSquaresX*2 + boardWidthSquares) * squareSizePixels).toString + "px"
  rendered.style.height = ((offsetInSquaresY*2 + boardHeightSquares) * squareSizePixels).toString + "px"

  def animateSquareMarkFailure(row: Int, col: Int): Unit = {
    val x = ((col + offsetInSquaresX) * squareSizePixels) + squareSizePixels / 2
    val y = ((row + offsetInSquaresY) * squareSizePixels) + squareSizePixels / 2

    val anim = AnimationParticleBurst("particle-failure", rendered, x, y, particleSettings, () => {
//      println("Animation done")
    })
    animations.append(anim)
  }

  private var particleSettings = ParticleSettings()

  def animateSquareMarkSuccess(row: Int, col: Int): Unit = {
    val x = ((col + offsetInSquaresX) * squareSizePixels) + squareSizePixels / 2
    val y = ((row + offsetInSquaresY) * squareSizePixels) + squareSizePixels / 2

    val anim = AnimationParticleBurst("particle-success", rendered, x, y, particleSettings, () => {
//      println("Animation done")
    })
    animations.append(anim)
  }

  def settingsChanged(newParticleSettings: ParticleSettings) = {
    particleSettings = newParticleSettings;
  }

  keepCheckingForFinishedAnimations()

  def keepCheckingForFinishedAnimations(): Unit = {
    if (particleSettings.animate == 1 && animations.nonEmpty && !animations.exists(_.done == false)) {
//      println("All animations done, deleting all")
      while (rendered.hasChildNodes()) rendered.removeChild(rendered.lastChild)
      animations.clear()
    }

    setTimeout(particleSettings.cleanupEveryXMecs) {
      keepCheckingForFinishedAnimations()
    }
  }
}

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

// Animations and particles need to exist independent of BoardStateView, which gets destroyed
class Overlay(val squareSizePixels: Int, boardWidthSquares: Int, boardHeightSquares: Int) {
  private val animations = ArrayBuffer.empty[Animation]

  // Give ourselves some padding so particles don't clip right off the edge
  private val offsetInSquaresX = 2
  private val offsetInSquaresY = 0
  private val overlay = div(cls := "overlay").render
  private val particles = div(cls := "particle-board").render
  private val countOffsetX: Float = squareSizePixels / 2
  private val countOffsetY: Float = squareSizePixels / 2

  val rendered = div(cls := "particles-and-overlays")(
    particles,
    overlay
  ).render

  rendered.style.top = (-offsetInSquaresY * squareSizePixels).toString + "px"
  rendered.style.left = (-offsetInSquaresX * squareSizePixels).toString + "px"

  rendered.style.width = ((offsetInSquaresX*2 + boardWidthSquares) * squareSizePixels).toString + "px"
  rendered.style.height = ((offsetInSquaresY*2 + boardHeightSquares) * squareSizePixels).toString + "px"

  particles.style.width = ((offsetInSquaresX*2 + boardWidthSquares) * squareSizePixels).toString + "px"
  particles.style.height = ((offsetInSquaresY*2 + boardHeightSquares) * squareSizePixels).toString + "px"

//  overlay.style.top = (-offsetInSquaresY * squareSizePixels).toString + "px"
//  overlay.style.left = (-offsetInSquaresX * squareSizePixels).toString + "px"

  overlay.style.width = ((offsetInSquaresX*2 + boardWidthSquares) * squareSizePixels).toString + "px"
  overlay.style.height = ((offsetInSquaresY*2 + boardHeightSquares) * squareSizePixels).toString + "px"

  overlay.onmouseup = (e) => {
    println("overlay onmouseup")
  }

  particles.onmouseup = (e) => {
    println("particles onmouseup")
  }

  rendered.onmouseup = (e) => {
    println("rendered onmouseup")
  }


  def getCoords(row: Int, col: Int): (Int,Int) = {
    val x = ((col + offsetInSquaresX) * squareSizePixels) + squareSizePixels / 2
    val y = ((row + offsetInSquaresY) * squareSizePixels) + squareSizePixels / 2
    (x, y)
  }

  def animateSquareMarkFailure(row: Int, col: Int): Unit = {
    val (x, y) = getCoords(row,col)
    animations.append(AnimationParticleBurst("particle-failure", particles, x, y, particleSettings, () => {}))
  }

  def animateSquareDeleteFailure(row: Int, col: Int): Unit = {
    val (x, y) = getCoords(row,col)
    animations.append(AnimationParticleBurst("particle-failure", particles, x, y, particleSettings, () => {}))
  }

  def animateSquareDeleteSuccess(row: Int, col: Int): Unit = {
    val (x, y) = getCoords(row,col)
    animations.append(AnimationParticleBurst("particle-delete-success", particles, x, y, particleSettings, () => {}))
  }

  private var particleSettings = ParticleSettings()

  def animateSquareMarkSuccess(row: Int, col: Int): Unit = {
    val x = ((col + offsetInSquaresX) * squareSizePixels) + squareSizePixels / 2
    val y = ((row + offsetInSquaresY) * squareSizePixels) + squareSizePixels / 2

    val anim = AnimationParticleBurst("particle-success", particles, x, y, particleSettings, () => {
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
      while (particles.hasChildNodes()) particles.removeChild(particles.lastChild)
      animations.clear()
    }

    setTimeout(particleSettings.cleanupEveryXMecs) {
      keepCheckingForFinishedAnimations()
    }
  }


  def drawOverlayHorizontal(start: Int, end: Int, row: Int): Unit = {
    clearOverlay()

    if (end >= start) {
      for (idx <- Range(start, end + 1)) {
        val view = div(cls := "square-overlay").render
        view.style.top = ((row + offsetInSquaresY) * squareSizePixels).toString + "px"
        view.style.left = ((idx + offsetInSquaresX) * squareSizePixels).toString + "px"
        overlay.appendChild(view)
      }

      val count = (end - start) + 1
      val view = div(cls := "count-overlay", count).render
      view.style.top = ((row + offsetInSquaresY) * squareSizePixels + countOffsetY).toString + "px"
      view.style.left = ((end + 1 + offsetInSquaresX) * squareSizePixels - countOffsetX).toString + "px"

      overlay.appendChild(view)
    }
    else {
      for (idx <- Range(end, start + 1)) {
        val view = div(cls := "square-overlay").render
        view.style.top = ((row + offsetInSquaresY) * squareSizePixels).toString + "px"
        view.style.left = ((idx + offsetInSquaresX) * squareSizePixels).toString + "px"
        overlay.appendChild(view)
      }

      val count = (start - end) + 1
      val view = div(cls := "count-overlay", count).render
      view.style.top = ((row + offsetInSquaresY) * squareSizePixels + countOffsetY).toString + "px"
      view.style.left = ((end + 1 + offsetInSquaresX) * squareSizePixels - countOffsetX).toString + "px"

      overlay.appendChild(view)
    }
  }

  def drawOverlayVertical(start: Int, end: Int, col: Int): Unit = {
    clearOverlay()

    if (end >= start) {
      for (idx <- Range(start, end + 1)) {
        val view = div(cls := "square-overlay").render
        view.style.left = ((col + offsetInSquaresX) * squareSizePixels).toString + "px"
        view.style.top = ((idx + offsetInSquaresY) * squareSizePixels).toString + "px"

        overlay.appendChild(view)
      }

      val count = (end - start) + 1
      val view = div(cls := "count-overlay", count).render
      view.style.left = ((col + 1 + offsetInSquaresX) * squareSizePixels - countOffsetX).toString + "px"
      view.style.top = ((end + offsetInSquaresY) * squareSizePixels + countOffsetY).toString + "px"

      overlay.appendChild(view)
    }
    else {
        for (idx <- Range(end, start + 1)) {
          val view = div(cls := "square-overlay").render
          view.style.left = ((col + offsetInSquaresX) * squareSizePixels).toString + "px"
          view.style.top = ((idx + offsetInSquaresY) * squareSizePixels).toString + "px"

          overlay.appendChild(view)
        }

        val count = (start - end) + 1
        val view = div(cls := "count-overlay", count).render
        view.style.left = ((col  + offsetInSquaresX) * squareSizePixels - countOffsetX).toString + "px"
        view.style.top = ((end + offsetInSquaresY) * squareSizePixels + countOffsetY).toString + "px"

        overlay.appendChild(view)
      }
  }

  def clearOverlay(): Unit = {
    while (overlay.hasChildNodes()) overlay.removeChild(overlay.lastChild)
  }
}

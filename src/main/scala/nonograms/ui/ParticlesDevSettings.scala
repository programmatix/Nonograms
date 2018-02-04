package nonograms.ui

import scalatags.JsDom.all._

case class TextInput(label: String, initial: String, onChange: () => Unit) {
  private val textInput = input(
    `type` := "text",
    value := initial
  ).render

  textInput.onchange = (e) => {
    onChange()
  }

  val rendered = div(
    div(label),
    textInput
  ).render

  def valueAsFloat = {
    var out = initial.toFloat
    try {
      out = textInput.value.toFloat
    }
    catch {
      case e: Throwable =>
    }
    out
  }

  def valueAsInt = {
    var out = initial.toInt
    try {
      out = textInput.value.toInt
    }
    catch {
      case e: Throwable =>
    }
    out
  }
}

class ParticlesDevSettings(onSettingsChanged: (ParticleSettings) => Unit) {

  private var settings = ParticleSettings()

  val uiGravity = TextInput("Gravity", settings.gravity.toString, updateSettings)
  val uiUpdatesPerSec = TextInput("Updates Per Sec", settings.updatesPerSec.toString, updateSettings)
  val uiNumParticles = TextInput("Num Particles", settings.numParticlesOnSuccess.toString, updateSettings)
  val maxVelocityX = TextInput("maxVelocityX", settings.maxVelocityX.toString, updateSettings)
  val maxVelocityY = TextInput("maxVelocityY", settings.maxVelocityX.toString, updateSettings)
  val lifetime = TextInput("Lifetime Msecs", settings.minLifetimeMsecs.toString, updateSettings)
  val animate = TextInput("Animate", settings.animate.toString, updateSettings)
  val cleanupEveryXMecs = TextInput("cleanupEveryXMecs", settings.cleanupEveryXMecs.toString, updateSettings)
  val updateDelta = TextInput("updateDelta", settings.updateDelta.toString, updateSettings)

  val rendered = div(cls := "particle-settings")(
    uiGravity.rendered,
    uiUpdatesPerSec.rendered,
    uiNumParticles.rendered,
    maxVelocityX.rendered,
    maxVelocityY.rendered,
    lifetime.rendered,
    animate.rendered,
    cleanupEveryXMecs.rendered,
    updateDelta.rendered
  ).render


  def updateSettings(): Unit = {
    settings = settings.copy(gravity = uiGravity.valueAsFloat,
      updatesPerSec = uiUpdatesPerSec.valueAsFloat,
      numParticlesOnSuccess = uiNumParticles.valueAsInt,
      numParticlesOnFailure = uiNumParticles.valueAsInt,
      maxVelocityX = maxVelocityX.valueAsFloat,
      maxVelocityY = maxVelocityY.valueAsFloat,
      minLifetimeMsecs = lifetime.valueAsInt,
      animate = animate.valueAsInt,
      cleanupEveryXMecs = cleanupEveryXMecs.valueAsInt,
      updateDelta = updateDelta.valueAsFloat
    )
    onSettingsChanged(settings)
  }
}

# Nonograms
An implementation of the puzzle game Nonograms, in Scala, transpiled to Javascript with ScalaJS.

The engine is completely immutable, and all components are nicely separated with no strong binding between UI and game logic.

## Status
Playable.

The solver AI is coming along and can find a solvable 10x10 puzzle usually given 20 random boards or less, but it could be a lot smarter. 

## Usage
Clone the project.

```sbt fastOptJS```

Then open src/main/resources/www/html/index.html in Chrome
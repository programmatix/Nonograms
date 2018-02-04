package nonograms

import scala.collection.mutable.ArrayBuffer

// Working with 2-dimensional arrays a lot, add some generic helpers
case class Vector2Dim[T](data: Vector[Vector[T]]) {
  val rows: Int = data.length
  val cols: Int = data(0).length

  def get(row: Int, col: Int): T = data(row)(col)

  def getRow(row: Int): Vector[T] = data(row)

  def getCol(col: Int): Vector[T] = Range(0, rows).map(row => get(row, col)).toVector

  def set(row: Int, col: Int, t: T): Vector2Dim[T] = {
    val copied = ArrayBuffer.empty[ArrayBuffer[T]]

    for (r <- Range(0, rows)) {
      val rowData = ArrayBuffer.empty[T]
      copied += rowData

      for (c <- Range(0, cols)) {
        if (row == r && col == c) {
          rowData.append(t)
        }
        else {
          rowData.append(get(r, c))
        }
      }
    }

    val mapped = Vector2Dim.arrayBufferToVector(copied)

    new Vector2Dim[T](mapped)
  }

  def flipHorizontally(): Vector2Dim[T] = {
    val copied = ArrayBuffer.empty[ArrayBuffer[T]]

    for (r <- Range(0, rows)) {
      val rowData = ArrayBuffer.empty[T]
      copied += rowData

      for (c <- Range(0, cols)) {
        val d = get(r, c)
        rowData.insert(0, d)
      }
    }

    val mapped = Vector2Dim.arrayBufferToVector(copied)

    new Vector2Dim[T](mapped)
  }

  def flipVertically(): Vector2Dim[T] = {
    val copied = ArrayBuffer.empty[ArrayBuffer[T]]

    for (r <- Range(0, rows)) {
      val rowData = ArrayBuffer.empty[T]

      for (c <- Range(0, cols)) {
        val d = get(r, c)
        rowData.append(d)
      }

      copied.insert(0, rowData)
    }

    val mapped = Vector2Dim.arrayBufferToVector(copied)

    new Vector2Dim[T](mapped)
  }
}

object Vector2Dim {
  def create[T](rows: Int, cols: Int, t: T) = {
    val table = Vector.fill(rows, cols)(t)
    new Vector2Dim[T](table)
  }

  def arrayBufferToVector[T](in: ArrayBuffer[ArrayBuffer[T]]): Vector[Vector[T]] = {
    in.map(row => {
      val x = row.toIndexedSeq.toVector
      x
    }).toIndexedSeq.toVector
  }

  def from[T](in: ArrayBuffer[ArrayBuffer[T]]): Vector2Dim[T] = {
    Vector2Dim(arrayBufferToVector(in))
  }
}

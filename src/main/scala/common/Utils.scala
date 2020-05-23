package common

import common.Common.{Color, Color8Bit, Pos3}

object Common {
  type Color8Bit = (Int, Int, Int)
  type Color = Vec3
  type Pos3 = Vec3
}

object Color{
  def apply(): Color = Vec3.create
  def apply(r: Double, g: Double, b: Double): Color = {
    Vec3.create(r, g, b)
  }
}

object Pos3{
  def create: Pos3 = Vec3.create
  def create(x: Double, y: Double, z: Double): Pos3 = Vec3.create(x,y,z)
}


object Utils {
  def normalizeColor(color: Color): Color8Bit = {
    val color8Bit = color.clamp(0, 0.999) * 256
    (color8Bit.x.toInt, color8Bit.y.toInt, color8Bit.z.toInt)
  }

  def colorToHex(color: Color): Int = colorToHex(normalizeColor(color))

  def colorToHex(color: Color8Bit): Int = {
    (color._1 & 0xff) * 0x010000 +
      (color._2 & 0xff) * 0x000100 +
      (color._3 & 0xff)
  }
}

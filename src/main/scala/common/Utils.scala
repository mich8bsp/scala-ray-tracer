package common

import common.Common.{Color, ColorDouble}

object Common {
  type Color = (Int, Int, Int)
  type ColorDouble = (Double, Double, Double)
  type Pos3 = Vec3
}


object Utils {
  def colorNormalizer(colorDouble: ColorDouble): Color = {
    ((colorDouble._1 * 255).toInt, (colorDouble._2 * 255).toInt, (colorDouble._3 * 255).toInt)
  }

  def colorToHex(color: Color): Int = {
    (color._1 & 0xff) * 0x010000 +
      (color._2 & 0xff) * 0x000100 +
      (color._3 & 0xff)
  }
}

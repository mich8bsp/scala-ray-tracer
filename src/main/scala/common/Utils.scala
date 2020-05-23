package common

import common.Common.Color

object Common {
  type Color = (Int, Int, Int)
  type Pos3 = Vec3
}


object Utils {
  def colorToHex(color: Color): Int = {
    (color._1 & 0xff) * 0x010000 +
      (color._2 & 0xff) * 0x000100 +
      (color._3 & 0xff)
  }
}

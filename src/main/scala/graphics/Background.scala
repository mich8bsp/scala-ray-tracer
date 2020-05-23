package graphics

import common.Color
import common.Common.Color

class Background {

  def getColor(ray: Ray): Option[Color] = {
    val unitDirection = ray.direction.unit
    val t = 0.5 * (unitDirection.y  + 1D)
    Some((Color(1D, 1D,1D) * (1D-t)) + (Color(0.5,0.7,1.0) * t))
  }
}

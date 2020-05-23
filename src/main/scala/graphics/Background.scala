package graphics

import common.Common.Color
import common.HittableObject

class Background extends HittableObject{

  override def isHitByRay(ray: Ray): Boolean = true

  override def getRayHitColor(ray: Ray): Option[Color] = {
//    val unitDirection = ray.direction.unit
//    val t = 0.5 * (unitDirection.y + 1.0)
//    Utils.colorNormalizer(1.0 - 0.5 * t, 1.0 - 0.3 * t, 1.0)
   Some((255, 255, 255))
  }
}

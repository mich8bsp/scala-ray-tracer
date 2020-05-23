package common

import common.Common.Color
import graphics.Ray

trait HittableObject {

  def isHitByRay(ray: Ray): Boolean
  def getRayHitColor(ray: Ray): Option[Color]
}

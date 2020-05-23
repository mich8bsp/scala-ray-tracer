package graphics

import common.Common.Color
import common.{Color, HittableObject, Vec3}

class Background extends HittableObject{

  override def hitWithRay(ray: Ray, tMin: Double, tMax: Double): Seq[RayHitData] = Seq(RayHitData(ray, Double.MaxValue, Vec3.Infinity, Vec3.create))

  override def isHitByRay(rayHitData: Seq[RayHitData]): Boolean = true

  override def getRayHitColor(rayHitData: RayHitData): Option[Color] = {
    val unitDirection = rayHitData.ray.direction.unit
    val t = 0.5 * (unitDirection.y  + 1D)
    Some((Color(1D, 1D,1D) * (1D-t)) + (Color(0.5,0.7,1.0) * t))
  }
}

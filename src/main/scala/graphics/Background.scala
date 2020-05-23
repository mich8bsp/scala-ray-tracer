package graphics

import common.{HittableObject, Utils, Vec3}

class Background extends HittableObject{

  override def hitWithRay(ray: Ray, tMin: Double, tMax: Double): Seq[RayHitData] = Seq(RayHitData(ray, Double.MaxValue, Vec3.Infinity, Vec3.create))

  override def isHitByRay(rayHitData: Seq[RayHitData]): Boolean = true

  override def getRayHitColor(rayHitData: RayHitData): Option[(Int, Int, Int)] = {
    val unitDirection = rayHitData.ray.direction.unit
    val t = 0.5 * (unitDirection.y  + 1D)
    Some(Utils.normalizeColor(
      (1.0-0.5*t, 1.0-0.3*t, 1.0)
    ))
  }
}

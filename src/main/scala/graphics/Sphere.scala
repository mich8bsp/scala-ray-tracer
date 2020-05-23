package graphics

import common.Common.{Color, Pos3}
import common.{HittableObject, Utils, Vec3}

case class Sphere(center: Pos3,
                  radius: Double) extends HittableObject {

  override def hitWithRay(ray: Ray, tMin: Double, tMax: Double): Seq[RayHitData] = {
    val A = ray.origin
    val C = center
    val b = ray.direction
    val AtoC = A - C

    val eqA: Double = b.lengthSquared
    //    val eqB = b * AtoC * 2D
    val eqH: Double = b * AtoC
    val eqC: Double = AtoC.lengthSquared - radius * radius
    val discriminant: Double = eqH * eqH - eqA * eqC
    Some(discriminant)
      .toSeq
      .filter(_ >= 0)
      .flatMap(discriminant => Seq(
        (-eqH - math.sqrt(discriminant)) / eqA,
        (-eqH + math.sqrt(discriminant)) / eqA
      ))
      .filter(t => t >= tMin && t <= tMax)
      .map(t => createHitPointData(ray, t))
  }

  private def createHitPointData(ray: Ray, t: Double): RayHitData = {
    val hitPoint: Pos3 = ray.at(t)
    val hitPointNormal: Vec3 = (hitPoint - center) / radius
    RayHitData(ray, t, hitPoint, hitPointNormal)
  }

  override def isHitByRay(rayHitData: Seq[RayHitData]): Boolean = rayHitData.nonEmpty

  override def getRayHitColor(rayHitData: RayHitData): Option[Color] = {
    val normal: Vec3 = rayHitData.hitPointNormal
    Some(Utils.normalizeColor(0.5 * normal.x + 0.5, 0.5 * normal.y + 0.5, 0.5 * normal.z + 0.5))
  }
}

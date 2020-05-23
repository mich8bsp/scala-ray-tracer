package common

import common.Common.{Color, Pos3}
import graphics.Ray

case class Sphere(center: Pos3,
                  radius: Double) extends HittableObject{

  /**
   * (P - C) * (P - C) = r^2 where C is the center of sphere, r is radius and P is point on surface
   * (A + t*b - C) * (A + t*b - C) = r^2
   * t^2 * b * b + 2t * b * (A-C) + (A-C)(A-C) - r^2 = 0
   *
   */
  override def isHitByRay(ray: Ray): Boolean = {
    val A = ray.origin
    val C = center
    val b = ray.direction
    val AtoC = A - C

    val eqA = b * b
    val eqB = b * AtoC * 2D
    val eqC = AtoC * AtoC - radius*radius
    val discriminant = eqB*eqB - eqA * eqC * 4D
    discriminant > 0
  }

  override def getRayHitColor(ray: Ray): Color = (255, 0, 0)
}

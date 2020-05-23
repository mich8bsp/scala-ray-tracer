package graphics

import common.Common.Pos3
import common.Vec3

case class Ray(
              origin: Pos3,
              direction: Vec3
              ){

  // P(t) = A + tb
  def at(t: Double): Vec3 = origin + (direction * t)

}

case class RayHitData(
                     ray: Ray,
                     t: Double,
                     hitPoint: Pos3,
                     hitPointOutNormal: Vec3
                     ){
  val frontFace: Boolean = ray.direction * hitPointOutNormal < 0
  val hitPointNormal: Vec3 = if(frontFace) hitPointOutNormal else -hitPointOutNormal
}

object Ray{

  def create(origin: Pos3, direction: Vec3): Ray = Ray(origin, direction)
}

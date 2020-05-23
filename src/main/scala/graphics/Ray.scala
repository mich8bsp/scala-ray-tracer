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

object Ray{

  def create(origin: Pos3, direction: Vec3): Ray = Ray(origin, direction)
}

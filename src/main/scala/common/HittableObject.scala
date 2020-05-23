package common

import common.Common.Color
import graphics.{Ray, RayHitData}

trait HittableObject {

  def hitWithRay(ray: Ray, tMin: Double = 0D, tMax: Double = Double.MaxValue): Seq[RayHitData]
  def isHitByRay(ray: Ray, tMin: Double = 0D, tMax: Double = Double.MaxValue): Boolean = {
    isHitByRay(hitWithRay(ray, tMin, tMax))
  }

  def isHitByRay(rayHitData: Seq[RayHitData]): Boolean
  def getRayHitColor(rayHitData: RayHitData): Option[Color]

  def getMaterial: Option[Material]
}

package graphics

import common.Common.{Color, Pos3}
import common.{HittableObject, Utils, Vec3}

case class Sphere(center: Pos3,
                  radius: Double) extends HittableObject{

  override def isHitByRay(ray: Ray): Boolean = {
    val t = getHitPointRayCoefficient(ray)
    t.isDefined
  }

  override def getRayHitColor(ray: Ray): Option[Color] = {
    val tOpt = getHitPointRayCoefficient(ray)
    tOpt.map(t => {
      val normal: Vec3 = (ray.at(t) - Vec3.create(0,0,-1)).unit
      Utils.normalizeColor(0.5*normal.x+0.5, 0.5*normal.y + 0.5, 0.5*normal.z + 0.5)
    })
  }

  def getHitPointRayCoefficient(ray: Ray): Option[Double] = {
    val A = ray.origin
    val C = center
    val b = ray.direction
    val AtoC = A - C

    val eqA: Double = b.lengthSquared
//    val eqB = b * AtoC * 2D
    val eqH: Double = b * AtoC
    val eqC: Double = AtoC.lengthSquared - radius*radius
    val discriminant: Double = eqH*eqH - eqA * eqC
    if(discriminant<0){
      None
    }else{
      Some((-eqH - math.sqrt(discriminant))/eqA)
    }
  }
}

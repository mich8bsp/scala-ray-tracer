package common

import common.Common.{Color, Pos3}
import common.Vec3.random
import graphics.RayHitData

trait Material {
  def scatterRay(rayHitData: RayHitData): Option[Pos3]
  def attenuate(color: Color): Color
}

trait DiffuseMaterial extends Material {
  val attenuationColor: Color

  override def attenuate(color: Color): Color = {
    color :*: attenuationColor
  }
}

case class DiffuseMaterialApproxLambert(attenuationColor: Color) extends DiffuseMaterial {

  override def scatterRay(rayHitData: RayHitData): Option[Pos3] = {
    Some(rayHitData.hitPoint + rayHitData.hitPointNormal + randomVecInUnitSphere)
  }

  private def randomVecInUnitSphere: Pos3 = {
    var found: Option[Vec3] = None
    while(found.isEmpty){
      val p = random(-1, 1)
      if(p.lengthSquared < 1){
        found = Some(p)
      }
    }
    found.get
  }
}

case class DiffuseMaterialTrueLambert(attenuationColor: Color) extends DiffuseMaterial {
  override def scatterRay(rayHitData: RayHitData): Option[Pos3] = {
    Some(rayHitData.hitPoint + rayHitData.hitPointNormal + Vec3.randomUnit)
  }
}

case class DiffuseMaterialAlt(attenuationColor: Color) extends DiffuseMaterial {
  override def scatterRay(rayHitData: RayHitData): Option[Pos3] = {
    val unitVector = Vec3.randomUnit
    val sameHemisphereAsNormal: Boolean = unitVector * rayHitData.hitPointNormal > 0D
    Some(rayHitData.hitPoint + {if(sameHemisphereAsNormal) unitVector else -unitVector})
  }
}

case class MetalMaterial(attenuationColor: Color, diffusion: Double) extends Material {
  override def scatterRay(rayHitData: RayHitData): Option[Pos3] = {
    val v = rayHitData.ray.direction
    val n = rayHitData.hitPointNormal
    val b = v * n
    Some(v - n * 2D*b + (Vec3.randomUnit * diffusion))
  }

  override def attenuate(color: Color): Color = color :*: attenuationColor
}
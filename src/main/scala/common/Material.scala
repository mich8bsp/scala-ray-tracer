package common

import common.Common.{Color, Pos3}
import common.Vec3.random
import graphics.RayHitData

import scala.util.Random

trait Material {
  def scatterRay(rayHitData: RayHitData): Option[Pos3]
  def attenuate(color: Color): Color
}

object Material{
  def reflectRay(rayDirection: Vec3, hitNormal: Vec3): Vec3 = {
    val v = rayDirection
    val n = hitNormal
    val b = v * n
    v - n * 2D*b
  }
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
  import Material._

  override def scatterRay(rayHitData: RayHitData): Option[Pos3] = {
    Some(reflectRay(rayHitData.ray.direction, rayHitData.hitPointNormal) + (Vec3.randomUnit * diffusion))
  }

  override def attenuate(color: Color): Color = color :*: attenuationColor
}

case class DielectricMaterial(refractionIndex: Double) extends Material {
  import Material._

  override def scatterRay(rayHitData: RayHitData): Option[Pos3] = {
    val refIdxRatio: Double = if(rayHitData.frontFace) 1D/refractionIndex else refractionIndex
    val rayDirUnit: Vec3 = rayHitData.ray.direction.unit
    val normal: Vec3 = rayHitData.hitPointNormal.unit
    val cosTheta: Double = math.min(-rayDirUnit * normal, 1D)
    val sinTheta: Double = math.sqrt(1D - cosTheta * cosTheta)
    lazy val mustReflect: Boolean = refIdxRatio * sinTheta > 1D
    lazy val canReflectByShlick = Random.between(0D, 1D) < shlickApprox(cosTheta, refIdxRatio)
    val isReflected: Boolean = mustReflect || canReflectByShlick

    val outRay: Vec3 = if(isReflected){
      reflectRay(rayHitData.ray.direction, rayHitData.hitPointNormal)
    }else{
      val rayOutParallel = (rayDirUnit + (normal * cosTheta)) * refIdxRatio
      val rayOutPerpend = normal * -math.sqrt(1D - rayOutParallel.lengthSquared)
      rayOutParallel + rayOutPerpend
    }

    Some(rayHitData.hitPoint + outRay)
  }

  private def shlickApprox(cosTheta: Double, refractionRatio: Double): Double = {
    val r0: Double = math.pow((1- refractionRatio) / (1+refractionRatio), 2)

    r0 + (1-r0)*math.pow(1D-cosTheta, 5)
  }

  override def attenuate(color: Color): Color = color
}
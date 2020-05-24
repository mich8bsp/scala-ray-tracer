package common

import common.Common.Pos3
import common.Vec3.random
import graphics.RayHitData

trait Material {
  def rayBounceTarget(rayHitData: RayHitData): Option[Pos3]
}

trait DiffuseMaterial extends Material {
  val diffuseRate: Double
}

case class DiffuseMaterialApproxLambert(diffuseRate: Double = 0.5) extends DiffuseMaterial {

  override def rayBounceTarget(rayHitData: RayHitData): Option[Pos3] = {
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

case class DiffuseMaterialTrueLambert(diffuseRate: Double = 0.5) extends DiffuseMaterial {
  override def rayBounceTarget(rayHitData: RayHitData): Option[Pos3] = {
    Some(rayHitData.hitPoint + rayHitData.hitPointNormal + Vec3.randomUnit)
  }
}

case class DiffuseMaterialAlt(diffuseRate: Double = 0.5) extends DiffuseMaterial {
  override def rayBounceTarget(rayHitData: RayHitData): Option[Pos3] = {
    val unitVector = Vec3.randomUnit
    val sameHemisphereAsNormal: Boolean = unitVector * rayHitData.hitPointNormal > 0D
    Some(rayHitData.hitPoint + {if(sameHemisphereAsNormal) unitVector else -unitVector})
  }
}
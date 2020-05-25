package graphics

import common.Common.Pos3
import common.{Pos3, Vec3}

case class Camera(origin: Pos3 = Pos3.create,
                  lookAt: Pos3,
                  vecUp: Vec3 = Vec3.create(0,1,0),
                  vertFOV: Double = 90D,
                  aspectRatio: Double = 16D/9D,
                  aperture: Double,
                  focusDistance: Double){
  outer =>

  private val h: Double = math.tan(math.toRadians(vertFOV) / 2D)

  private val w: Vec3 = (origin - lookAt).unit
  private val u: Vec3 = (vecUp x w).unit
  private val v: Vec3 = w x u

  private val lensRadius: Double = aperture / 2D

  private val viewport: Viewport = Viewport(
    width = 2D * h * aspectRatio,
    height = 2D * h,
    orientation = (u,v,w),
    focusDistance = focusDistance
  )

  def getRay(targetHorizPerc: Double, verticalVertPer: Double): Ray = {
    val rd: Vec3 = Vec3.randomInUnitDisk *  lensRadius
    val offset: Vec3 = u * rd.x + v * rd.y
    val originOnLens: Pos3 = origin + offset
    Ray.create(originOnLens, viewport.getPosByPercentage(targetHorizPerc, verticalVertPer) - originOnLens)
  }

  case class Viewport(
                       width: Double,
                       height: Double,
                       orientation: (Vec3, Vec3, Vec3),
                       focusDistance: Double
                     ){
    private val horizontal = orientation._1 * width * focusDistance
    private val vertical = orientation._2 * height * focusDistance
    private val lowerLeftCorner: Pos3 = outer.origin - horizontal / 2 - vertical / 2 - (orientation._3 * focusDistance)

    def getPosByPercentage(horizontalPercentage: Double, verticalPercentage: Double): Pos3 = {
      lowerLeftCorner + horizontal * horizontalPercentage + vertical * verticalPercentage
    }
  }
}




package graphics

import common.Common.Pos3
import common.{Pos3, Vec3}

case class Camera(origin: Pos3 = Pos3.create,
                  lookAt: Pos3,
                  vecUp: Vec3 = Vec3.create(0,1,0),
                  vertFOV: Double = 90D,
                  aspectRatio: Double = 16D/9D){
  outer =>

  private val h: Double = math.tan(math.toRadians(vertFOV) / 2D)

  private val w: Vec3 = (origin - lookAt).unit
  private val u: Vec3 = vecUp x w
  private val v: Vec3 = w x u

  private val viewport: Viewport = Viewport(
    width = 2D * h * aspectRatio,
    height = 2D * h,
    orientation = (u,v,w)
  )

  def getRay(targetHorizPerc: Double, verticalVertPer: Double): Ray = {
    Ray.create(origin, viewport.getPosByPercentage(targetHorizPerc, verticalVertPer) - origin)
  }

  case class Viewport(
                       width: Double,
                       height: Double,
                       orientation: (Vec3, Vec3, Vec3)
                     ){
    private val horizontal = orientation._1 * width
    private val vertical = orientation._2 * height
    private val lowerLeftCorner: Pos3 = outer.origin - horizontal / 2 - vertical / 2 - orientation._3

    def getPosByPercentage(horizontalPercentage: Double, verticalPercentage: Double): Pos3 = {
      lowerLeftCorner + horizontal * horizontalPercentage + vertical * verticalPercentage
    }
  }
}




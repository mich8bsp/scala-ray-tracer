package graphics

import common.Common.Pos3
import common.{Pos3, Vec3}

case class Camera(origin: Pos3 = Pos3.create,
                  aspectRatio: Double = 16D/9D,
                  viewportHeight: Double = 2D,
                  focalLength: Double = 1D){
  outer =>

  val viewport: Viewport = Viewport(
    width = viewportHeight * aspectRatio,
    height = viewportHeight
  )

  def getRay(targetHorizPerc: Double, verticalVertPer: Double): Ray = {
    Ray.create(origin, viewport.getPosByPercentage(targetHorizPerc, verticalVertPer))
  }

  case class Viewport(
                       width: Double,
                       height: Double
                     ){
    private val horizontal = Vec3.create(width, 0, 0)
    private val vertical = Vec3.create(0,height, 0)
    private val lowerLeftCorner: Pos3 = outer.origin - horizontal / 2 - vertical / 2 - Vec3.create(0,0, outer.focalLength)

    def getPosByPercentage(horizontalPercentage: Double, verticalPercentage: Double): Pos3 = {
      lowerLeftCorner + horizontal * horizontalPercentage + vertical * verticalPercentage
    }
  }
}




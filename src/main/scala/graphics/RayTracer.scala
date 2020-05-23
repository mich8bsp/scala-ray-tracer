package graphics

import common.Common.{Color, Pos3}
import common.{Pos3, Vec3}
import file.ImageWriter

class RayTracer(cameraPos: Pos3, viewport: Viewport, scene: Scene) {

  def render(imageWidth: Int, imageHeight: Int): Array[Array[Color]] = {
    (0 until imageHeight).reverse.map(j => {
      (0 until imageWidth).map(i  => {
        val horizontalPerc: Double = i.toDouble / imageWidth
        val verticalPerc: Double = j.toDouble / imageHeight

        val viewportPoint: Pos3 = viewport.getPosByPercentage(horizontalPerc, verticalPerc)
        val rayCameraToViewportPoint: Ray = Ray.create(cameraPos, viewportPoint - cameraPos)
        trace(rayCameraToViewportPoint)
      }).toArray
    }).toArray
  }

  private def trace(r: Ray): Color = {
    scene.sceneObjects
      .filter(_.isHitByRay(r))
      .flatMap(_.getRayHitColor(r))
      .lastOption
      .getOrElse((0,0,0))
  }

}

case class Viewport(
                     width: Double,
                     height: Double,
                     focalLength: Double,
                     cameraOrigin: Pos3
                   ){
  private val horizontal = Vec3.create(width, 0, 0)
  private val vertical = Vec3.create(0,height, 0)
  private val lowerLeftCorner: Pos3 = cameraOrigin - horizontal / 2 - vertical / 2 - Vec3.create(0,0,focalLength)

  def getPosByPercentage(horizontalPercentage: Double, verticalPercentage: Double): Pos3 = {
    lowerLeftCorner + horizontal * horizontalPercentage + vertical * verticalPercentage
  }
}

object RayTracerMain{
  def main(args: Array[String]): Unit = {
    val aspectRatio: Double = 16D/9D
    val imageWidth: Int = 384
    val imageHeight: Int = (imageWidth / aspectRatio).toInt
    val viewportHeight = 2D
    val viewportWidth = aspectRatio * viewportHeight
    val focalLength = 1D
    val cameraOrigin = Pos3.create

    val viewport = Viewport(
      width = viewportWidth,
      height = viewportHeight,
      focalLength = focalLength,
      cameraOrigin = cameraOrigin
    )

    val scene = new Scene()
      .addToScene(new Background)
      .addToScene(Sphere(center = Pos3.create(0, 0, -1), radius = 0.5))

    val tracer = new RayTracer(cameraOrigin, viewport, scene)

    val imageBuffer: Array[Array[Color]] = tracer.render(imageWidth, imageHeight)
    ImageWriter.writeImageToFile(imageBuffer, "tutorial_image.jpg")
  }
}

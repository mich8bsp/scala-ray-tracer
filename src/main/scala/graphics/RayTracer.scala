package graphics

import common.Common.Color
import common.{Color, HittableObject, Pos3}
import file.ImageWriter

import scala.util.Random

class RayTracer(camera: Camera, scene: Scene, sampleSize: Int) {

  def render(imageWidth: Int, imageHeight: Int): Array[Array[Color]] = {
    (0 until imageHeight).reverse.map(j => {
      (0 until imageWidth).map(i  => {
        val sampleColors: Seq[Color] = (0 until sampleSize).map(_ => {
        val horizontalPerc: Double = (i.toDouble + Random.between(0, 1D)) / imageWidth
        val verticalPerc: Double = (j.toDouble + Random.between(0, 1D)) / imageHeight

        val ray: Ray = camera.getRay(horizontalPerc, verticalPerc)
        trace(ray)
        })

        val sumColor: Color = sampleColors.foldLeft(Color())(_ + _)
        sumColor/sampleSize
      }).toArray
    }).toArray
  }

  private def trace(r: Ray): Color = {
    val objectsHitData: Seq[(HittableObject, RayHitData)] = scene.sceneObjects
        .map(obj => (obj, obj.hitWithRay(r)))
      .filter({
        case (obj, hitData) => obj.isHitByRay(hitData)
      })
      .flatMap({
        case (obj, hitData) => hitData.map((obj, _))
      })
        .toSeq

      Some(objectsHitData)
      .filter(_.nonEmpty)
      .map(_.minBy(_._2.t))
      .flatMap({
        case (obj, hitData) => obj.getRayHitColor(hitData)
      })
      .getOrElse(Color())
  }

}


object RayTracerMain{
  def main(args: Array[String]): Unit = {
    val aspectRatio: Double = 16D/9D
    val imageWidth: Int = 600
    val imageHeight: Int = (imageWidth / aspectRatio).toInt
    val sampleSize: Int = 100

    val camera = Camera(aspectRatio = aspectRatio)

    val scene = new Scene()
      .addToScene(new Background)
      .addToScene(Sphere(center = Pos3.create(0, 0, -1), radius = 0.5))
      .addToScene(Sphere(center = Pos3.create(0, -100.5, -1), radius = 100))

    val tracer = new RayTracer(camera, scene, sampleSize)

    val imageBuffer: Array[Array[Color]] = tracer.render(imageWidth, imageHeight)
    ImageWriter.writeImageToFile(imageBuffer, "tutorial_image.jpg")
  }
}

package graphics

import common.Common.{Color, Pos3}
import common.{Color, DielectricMaterial, DiffuseMaterialApproxLambert, DiffuseMaterialTrueLambert, HittableObject, MetalMaterial, Pos3, Vec3}
import file.ImageWriter

import scala.collection.mutable
import scala.util.Random

class RayTracer(camera: Camera, scene: Scene) {

  def render(imageWidth: Int, imageHeight: Int): mutable.Buffer[mutable.Buffer[Color]] = {
    (0 until imageHeight).reverse.map(j => {
      (0 until imageWidth).map(i  => {
        if(RayTracerConfig.ANTI_ALIASING){
          getAAColorForPixel(i,j,imageWidth, imageHeight)
        }else{
          getColorForPixel(i,j,imageWidth, imageHeight)
        }
      }).toBuffer
    }).toBuffer
  }

  private def getColorForPixel(i: Int, j: Int, width: Int, height: Int): Color = {
    val horizontalPerc: Double = i.toDouble / width
    val verticalPerc: Double = j.toDouble / height

    val ray: Ray = camera.getRay(horizontalPerc, verticalPerc)
    trace(ray)
  }

  private def getAAColorForPixel(i: Int, j: Int, width: Int, height: Int): Color = {
    val samplingSize = RayTracerConfig.ANTI_ALIASING_SAMPLING_SIZE
    val sampleColors: Seq[Color] = (0 until samplingSize).map(_ => {
      val horizontalPerc: Double = (i.toDouble + Random.between(0, 1D)) / width
      val verticalPerc: Double = (j.toDouble + Random.between(0, 1D)) / height

      val ray: Ray = camera.getRay(horizontalPerc, verticalPerc)
      trace(ray)
    })

    sampleColors.foldLeft(Color())(_ + _) /samplingSize
  }

  private def trace(r: Ray, bounceDepth: Int = 50): Color = {
    if(bounceDepth<=0){
      //ray exceeded bounce limit, no light gathered
      Color()
    }else{
      val objectsHitData: Seq[(HittableObject, RayHitData)] = hitSceneWithRay(r, scene)
      val closestObjectHit: Option[(HittableObject, RayHitData)] = Some(objectsHitData)
        .filter(_.nonEmpty)
        .map(_.minBy(_._2.t))

      val rayHitColor: Option[Color] = closestObjectHit
        .flatMap({
          case (obj, hitData) => {
            bounceRayOffObject(obj, hitData, bounceDepth)
              .map(Some(_))
              .getOrElse(obj.getRayHitColor(hitData))
          }
        })

        rayHitColor.getOrElse(scene.background.flatMap(_.getColor(r)).getOrElse(Color()))
    }
  }

  private def hitSceneWithRay(ray: Ray, scene: Scene): Seq[(HittableObject, RayHitData)] = {
    scene.sceneObjects
      .map(obj => (obj, obj.hitWithRay(ray)))
      .filter({
        case (obj, hitData) => obj.isHitByRay(hitData)
      })
      .flatMap({
        case (obj, hitData) => hitData.map((obj, _))
      })
      .toSeq
  }

  private def bounceRayOffObject(obj: HittableObject, hitData: RayHitData, bounceDepth: Int): Option[Color] = obj.getMaterial match {
    case Some(mat) =>
      val bounceTargetOpt: Option[Pos3] = mat.scatterRay(hitData)
      val bounceRayOpt: Option[Ray] = bounceTargetOpt.map(target => Ray.create(hitData.hitPoint, target - hitData.hitPoint))
      bounceRayOpt.map(bounceRay => mat.attenuate(trace(bounceRay, bounceDepth-1)))
    case _ => None
  }

}


object RayTracerMain{
  def main(args: Array[String]): Unit = {
    println("Starting Ray Tracer")
    val aspectRatio: Double = 16D/9D
    val imageWidth: Int = 348
    val imageHeight: Int = (imageWidth / aspectRatio).toInt

    println("Creating camera")
    val camera = Camera(aspectRatio = aspectRatio)

    println("Creating scene")
    val scene = new Scene()
      .setBackground(new Background)
      .addToScene(Sphere(center = Pos3.create(0, 0, -1), radius = 0.5).withMaterial(DiffuseMaterialTrueLambert(Color(0.7, 0.3, 0.3))))
      .addToScene(Sphere(center = Pos3.create(1, 0, -1), radius = 0.5).withMaterial(DielectricMaterial(1.5)))
      .addToScene(Sphere(center = Pos3.create(1, 0, -1), radius = -0.45).withMaterial(DielectricMaterial(1.5)))
      .addToScene(Sphere(center = Pos3.create(-1, 0, -1), radius = 0.4).withMaterial(MetalMaterial(Color(0.8,0.8,0.8), diffusion = 0.1)))
      .addToScene(Sphere(center = Pos3.create(0, -100.5, -1), radius = 100).withMaterial(DiffuseMaterialApproxLambert(Color(0.8, 0.8, 0D))))

    val tracer = new RayTracer(camera, scene)

    println("Start rendering")
    var start = System.currentTimeMillis()
    val imageBuffer: mutable.Buffer[mutable.Buffer[Color]] = tracer.render(imageWidth, imageHeight)
    println(s"Finished rendering, took ${(System.currentTimeMillis() - start)/1E3} sec")
    val postProcessor = new PostProcessor

    println(s"Start post-processing")
    start = System.currentTimeMillis()
    val postProcessBuffer: mutable.Buffer[mutable.Buffer[Color]] = postProcessor.process(imageBuffer)
    println(s"Finished post-processing, took ${(System.currentTimeMillis() - start)/1E3} sec")

    println(s"Start writing image to file")
    start = System.currentTimeMillis()
    ImageWriter.writeImageToFile(postProcessBuffer, "tutorial_image.jpg")
    println(s"Finished writing image to file, took ${(System.currentTimeMillis() - start)/1E3} sec")
    println(s"Process finished, exiting")
  }
}

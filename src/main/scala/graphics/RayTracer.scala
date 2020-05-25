package graphics

import java.util.concurrent.Executors

import common.Common.{Color, Pos3}
import common.{Color, HittableObject, Pos3}
import file.ImageWriter

import scala.collection.mutable
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.Random

class RayTracer(camera: Camera, scene: Scene)
               (implicit ec: ExecutionContext){

  def render(imageWidth: Int, imageHeight: Int): mutable.Buffer[mutable.Buffer[Color]] = {
    val pixelsToRender = imageHeight * imageWidth
    var lastProgress: Int = -1
    val res = (0 until imageHeight).reverse.map(j => {
      (0 until imageWidth).map(i  => {
        val pixelsProcessed: Int = (imageHeight-1-j)*imageWidth + i
        val currProgress = ((pixelsProcessed.toDouble * 100D)/pixelsToRender.toDouble).toInt
        if(currProgress!=lastProgress){
          println(s"$currProgress%")
          lastProgress = currProgress
        }
        if(RayTracerConfig.ANTI_ALIASING){
          Await.result(getAAColorForPixel(i,j,imageWidth, imageHeight), Duration.Inf)
        }else{
          getColorForPixel(i,j,imageWidth, imageHeight)
        }
      }).toBuffer
    }).toBuffer

    if(lastProgress!=100){
      println(s"100%")
    }
    res
  }

  private def getColorForPixel(i: Int, j: Int, width: Int, height: Int): Color = {
    val horizontalPerc: Double = i.toDouble / width
    val verticalPerc: Double = j.toDouble / height

    val ray: Ray = camera.getRay(horizontalPerc, verticalPerc)
    trace(ray)
  }

  private def getAAColorForPixel(i: Int, j: Int, width: Int, height: Int): Future[Color] = {
    val samplingSize = RayTracerConfig.ANTI_ALIASING_SAMPLING_SIZE
    val sampleColors: Future[Seq[Color]] = Future.sequence((0 until samplingSize).map(_ => {
      val horizontalPerc: Double = (i.toDouble + Random.between(0, 1D)) / width
      val verticalPerc: Double = (j.toDouble + Random.between(0, 1D)) / height

      val ray: Ray = camera.getRay(horizontalPerc, verticalPerc)
      Future {
        trace(ray)
      }
    }))

    sampleColors.map(_.foldLeft(Color())(_ + _) /samplingSize)
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

    implicit val ec: ExecutionContext = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(RayTracerConfig.ANTI_ALIASING_SAMPLING_SIZE))

    println("Starting Ray Tracer")
    val aspectRatio: Double = 16D/9D
    val imageWidth: Int = 348
    val imageHeight: Int = (imageWidth / aspectRatio).toInt

    println("Creating camera")
    val origin = Pos3.create(13,2,3)
    val target = Pos3.create(0,0,0)
    val distToFocus = 10D
    val aperture = 0.1
    val vertFOV = 20
    val camera = Camera(aspectRatio = aspectRatio,
      origin = origin,
      lookAt = target,
      focusDistance = distToFocus,
      aperture = aperture,
      vertFOV = vertFOV)

    println("Creating scene")
    val scene = SceneGenerator.generate

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

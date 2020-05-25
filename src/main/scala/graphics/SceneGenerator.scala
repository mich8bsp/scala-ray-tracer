package graphics

import common.{Color, DielectricMaterial, DiffuseMaterialApproxLambert, DiffuseMaterialTrueLambert, MetalMaterial, Pos3, Vec3}

import scala.util.Random

object SceneGenerator {

  def generateSmallPredefined: Scene = {
   new Scene()
      .setBackground(new Background)
      .addToScene(Sphere(center = Pos3.create(0, 0, -1), radius = 0.5).withMaterial(DiffuseMaterialTrueLambert(Color(0.7, 0.3, 0.3))))
      .addToScene(Sphere(center = Pos3.create(1, 0, -1), radius = 0.5).withMaterial(DielectricMaterial(1.5)))
      .addToScene(Sphere(center = Pos3.create(1, 0, -1), radius = -0.45).withMaterial(DielectricMaterial(1.5)))
      .addToScene(Sphere(center = Pos3.create(0.25, 0, -0.2), radius = 0.3).withMaterial(MetalMaterial(Color(0.8,0.6,0.2), diffusion = 0.3)))
      .addToScene(Sphere(center = Pos3.create(-1, 0, -1), radius = 0.4).withMaterial(MetalMaterial(Color(0.8,0.8,0.8), diffusion = 0.1)))
      .addToScene(Sphere(center = Pos3.create(0, -100.5, -1), radius = 100).withMaterial(DiffuseMaterialApproxLambert(Color(0.8, 0.8, 0D))))
  }

  def generate: Scene = {

    val ground = Sphere(Pos3.create(0, -1000, 0), 1000).withMaterial(DiffuseMaterialTrueLambert(Color(0.5, 0.5, 0.5)))

    val scene = new Scene()
      .setBackground(new Background)
      .addToScene(ground)

    val refPoint = Vec3.create(4, 0.2, 0)
    val smallSpheres: Seq[Sphere] = (for{
      a <- -11 until 11
      b <- -11 until 11
    }yield{
      val chooseMat = randomDouble
      val center = Pos3.create(a + 0.9 * randomDouble, 0.2, b + 0.9*randomDouble)

      if((center - refPoint).length > 0.9){
        if(chooseMat < 0.8){
          //diffuse
          Some(Sphere(center,0.2)
            .withMaterial(DiffuseMaterialTrueLambert(Color.random :*: Color.random)))
        }else if(chooseMat<0.95){
          //metal
          Some(Sphere(center, 0.2)
          .withMaterial(MetalMaterial(Color.random / 2D + 0.5, randomDouble/2D)))
        }else{
          //glass
          Some(Sphere(center, 0.2)
          .withMaterial(DielectricMaterial(Random.between(1.3, 1.5))))
        }
      }else{
        None
      }
    }).flatten

    val largeSpheres: Seq[Sphere] = Seq(
      Sphere(Pos3.create(0,1,0), 1D).withMaterial(DielectricMaterial(1.5)),
      Sphere(Pos3.create(-4,1,0), 1D).withMaterial(DiffuseMaterialTrueLambert(Color(0.4, 0.2, 0.1))),
      Sphere(Pos3.create(4,1,0), 1D).withMaterial(MetalMaterial(Color(0.7, 0.6, 0.5), 0D))
    )

    (smallSpheres ++ largeSpheres).foldLeft(scene)((scene, sphere) => scene.addToScene(sphere))

  }

  private def randomDouble: Double = Random.between(0D, 1D)
}

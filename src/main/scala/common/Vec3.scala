package common

import scala.util.Random

case class Vec3(x: Double,
                y: Double,
                z: Double){

  import Vec3._

  def apply(): Vec3 = create

  def unary_- : Vec3 = create(-x, -y, -z)

  def +(u: Vec3): Vec3 = create(x + u.x, y + u.y, z + u.z)
  def +(t: Double): Vec3 = create(x + t, y + t, z + t)
  def -(u: Vec3): Vec3 = create(x - u.x, y - u.y, z - u.z)
  def -(t: Double): Vec3 = this.+(-t)
  def *(t: Double): Vec3 = create(x*t, y*t, z*t)
  def /(t: Double): Vec3 = if(t!=0) *(1/t) else throw new IllegalArgumentException("Can't divide vector by 0")
  def *(u: Vec3): Double = x*u.x + y*u.y + z*u.z
  def :*:(other: Vec3): Vec3 = Vec3.create(
    x * other.x, y * other.y, z * other.z
  )
  def x(u: Vec3): Vec3 = create(
      y * u.z - z * u.y,
      z * u.x - x * u.z,
      x * u.y - y * u.x
    )


  def length: Double = math.sqrt(lengthSquared)
  def lengthSquared: Double = x*x + y*y + z*z

  def unit: Vec3 = this / length
  def clamp(minVal: Int, maxVal: Double): Vec3 = Vec3.create(
    math.max(math.min(x, maxVal), minVal),
    math.max(math.min(y, maxVal), minVal),
    math.max(math.min(z, maxVal), minVal)
  )

  def sqrt: Vec3 = Vec3.create(
    math.sqrt(x), math.sqrt(y), math.sqrt(z)
  )

}

object Vec3{
  val Infinity: Vec3 = create(Double.MaxValue, Double.MaxValue, Double.MaxValue)

  def create: Vec3 = create(0D,0D,0D)
  def create(x: Double, y: Double, z: Double): Vec3 = {
    //TODO: object pool?
    Vec3(x,y,z)
  }

  def random: Vec3 = random(0D, 1D)
  def random(min: Double, max: Double): Vec3 = Vec3.create(Random.between(min, max), Random.between(min, max), Random.between(min, max))
  def randomInUnitDisk: Vec3 = {
    val x: Double = Random.between(0D, 1D)
    val z: Double = 0D
    val maxY: Double = math.sqrt(1 - x*x - z*z)
    val y: Double = Random.between(0, maxY)
    Vec3.create(x,y,z)
  }

  def randomUnit: Vec3 = {
      val phi: Double = Random.between(0, 2D * math.Pi)
      val z: Double = Random.between(-1D, 1D)
      val r: Double = math.sqrt(1D - z*z)

      Vec3.create(r*math.cos(phi), r*math.sin(phi), z)
  }
}
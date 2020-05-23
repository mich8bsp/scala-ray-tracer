package common

import common.Common.Pos3

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

  def x(u: Vec3): Vec3 = create(
      y * u.z - z * u.y,
      z * u.x - x * u.z,
      x * u.y - y * u.x
    )


  def length: Double = math.sqrt(lengthSquared)
  def lengthSquared: Double = x*x + y*y + z*z

  def unit: Vec3 = this / length

}

object Pos3{
  def create: Pos3 = Vec3.create
  def create(x: Double, y: Double, z: Double): Pos3 = Vec3.create(x,y,z)
}

object Vec3{
  def create: Vec3 = create(0D,0D,0D)
  def create(x: Double, y: Double, z: Double): Vec3 = {
    //TODO: object pool?
    Vec3(x,y,z)
  }
}
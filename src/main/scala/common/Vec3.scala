package common

case class Vec3(x: Double,
                y: Double,
                z: Double){

  def apply(): Vec3 = Vec3(0,0,0)

  def unary_- : Vec3 = Vec3(-x, -y, -z)

  def +(u: Vec3): Vec3 = Vec3(x + u.x, y + u.y, z + u.z)
  def +(t: Double): Vec3 = Vec3(x + t, y + t, z + t)
  def -(u: Vec3): Vec3 = Vec3(x - u.x, y - u.y, z - u.z)
  def -(t: Double): Vec3 = this.+(-t)
  def *(t: Double): Vec3 = Vec3(x*t, y*t, z*t)
  def /(t: Double): Vec3 = *(1/t)
  def *(u: Vec3): Double = x*u.x + y*u.y + z*u.z

  def x(u: Vec3): Vec3 = {
    Vec3(
      y * u.z - z * u.y,
      z * u.x - x * u.z,
      x * u.y - y * u.x
    )
  }

  def length: Double = math.sqrt(lengthSquared)
  def lengthSquared: Double = x*x + y*y + z*z

  def unit: Vec3 = this / length

}

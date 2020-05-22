class Vect(var x: Double, var y: Double, var name: String = "", var index: Int = -1){
  
  def normalize() : Unit = {
    val norm: Double = Math.sqrt(x*x + y*y)
    x = x/norm
    y = y/norm
  }
  
  def setCoordinates(new_x: Double, new_y: Double) : Unit = {
	  x = new_x
	  y = new_y
  }
  
  def named(nm: String): Unit = {
    name = nm
  }
  
  def setIndex(ind: Int): Unit = {
	  index = ind
  }
  
  override def toString = x.toString().concat(" ".concat(y.toString()))
}

class vect(xc: Double, yc: Double){
  var x: Double = xc
  var y: Double = yc
  var name: String = ""
  var index: Int = -1
  
  def normalize(): Unit = {
    val norm: Double = Math.sqrt(x*x + y*y)
    x = x/norm
    y = y/norm
  }
  
  def named(nm: String): Unit = {
    name = nm
  }
  
  def setIndex(ind: Int): Unit = {
	  index = ind
  }
  
  override def toString = x.toString().concat(" ".concat(y.toString()))
}

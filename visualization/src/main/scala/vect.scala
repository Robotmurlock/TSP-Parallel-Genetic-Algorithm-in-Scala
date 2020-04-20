class vect(xc: Double, yc: Double){
  var x: Double = xc
  var y: Double = yc
  var name: String = ""
  var index: Int = -1
  
  def normalize(){
    var norm: Double = Math.sqrt(x*x + y*y)
    x = x/norm
    y = y/norm
  }
  
  def named(nm: String){
    name = nm
  }
  
  def setIndex(ind: Int){
	  index = ind
  }
  
  override def toString = x.toString().concat(" ".concat(y.toString()))
}

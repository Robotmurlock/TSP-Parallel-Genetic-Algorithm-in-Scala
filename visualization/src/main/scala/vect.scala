class vect(xc: Double, yc: Double){
  var x: Double = xc
  var y: Double = yc
  var name: String = ""
  
  def normalize(){
    var norm: Double = Math.sqrt(x*x + y*y)
    x = x/norm
    y = y/norm
  }
  
  def named(nm: String){
    name = nm
  }
  
  override def toString = x.toString().concat(" ".concat(y.toString()))
}

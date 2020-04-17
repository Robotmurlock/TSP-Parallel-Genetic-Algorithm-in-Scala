class vect(xc: Double, yc: Double){
  var x: Double = xc
  var y: Double = yc 
  
  def normalize(){
    var norm: Double = Math.sqrt(x*x + y*y)
    x = x/norm
    y = y/norm
  }
  
  override def toString = x.toString().concat(" ".concat(y.toString()))
}
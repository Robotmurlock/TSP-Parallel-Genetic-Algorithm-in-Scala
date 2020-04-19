import scala.io.Codec
import java.nio.charset.CodingErrorAction
object CSVReader{
  def main (args: Array[String]): Unit = {
    implicit val codec = Codec("UTF-8")
    codec.onMalformedInput(CodingErrorAction.REPLACE)
    codec.onUnmappableCharacter(CodingErrorAction.REPLACE)
	//treba promeniti odakle uzima .csv fajl jer sam radio na vm za pp
    val bufferedSource = io.Source.fromFile("/home/student/TSP-Parallel-Genetic-Algorithm-in-Scala/genetic_algorithm/src/main/tsp_examples/Gradovi.csv")
    val cost_matrix = Array.ofDim[String](100, 100)
    var j:Int=0
    var k:Int=0
    for (line <- bufferedSource.getLines()) {
	// ako ne radi kako treba sledeca linija :
      val cols = line.split(",").map(_.trim)
	// onda samo otkomentarisati:
	//val cols = line.split(";").map(_.trim)
      for (i <- cols) {
        cost_matrix(j)(k)=i
        k=k+1
      }
      k=0
      j=j+1
    }
    for(i<-0 until j) {
      for(k<-0 until j){
        print(cost_matrix(i)(k)+" ")
      }
      println("")
    }

  }


}

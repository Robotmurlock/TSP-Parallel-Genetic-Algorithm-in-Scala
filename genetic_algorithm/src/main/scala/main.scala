import scala.io.Source
import java.util.Scanner
import java.io.File
import genetic_algorithm.GA
import scala.io.Codec
import java.nio.charset.CodingErrorAction

object CSVReader{
    val delimiter = ";"

    def read_from_csv(file: String) : Array[Array[Double]] = {
        implicit val codec = Codec("UTF-8")
        codec.onMalformedInput(CodingErrorAction.REPLACE)
        codec.onUnmappableCharacter(CodingErrorAction.REPLACE)
        val bufferedSource = io.Source.fromFile(file)
        var cost_matrix: Array[Array[Double]] = null
        var j: Int = 0
        var k: Int = 0
        var first_line: Boolean = true
        for (line <- bufferedSource.getLines()) {
            if(!first_line) {
                var cols = line.split(delimiter).map(_.trim)
                // skips first columns
                cols = cols.slice(1, cols.size)

                // memory optimization
                if(cost_matrix == null) {
                    cost_matrix = Array.ofDim[Double](cols.size, cols.size)
                }

                for (cell <- cols) {
                    cost_matrix(j)(k) = cell.toDouble
                    k += 1
                }
                k = 0
                j += 1
            }
            else {
                // skips header
                first_line = false
            }
        }
        for(i <- 0 until j) {
            for(k<-0 until j){
                print(cost_matrix(i)(k) + " ")
            }
            println("")
        }
        return(cost_matrix)
    }
}

object Main {
    def main(args: Array[String]) : Unit = {
        val prefix = "src/main/tsp_examples/"
        val filename = prefix + args(0)
        var log: Boolean = false

        var cost_matrix: Array[Array[Double]] = null
        if(args(1) == "-as-csv") {
            cost_matrix = CSVReader.read_from_csv(filename)
        }
        else {
            val sc = new Scanner(new File(filename))
            val dimension = sc.nextInt()
            cost_matrix = Array.ofDim[Double](dimension, dimension)

            for(i <- 0 to dimension-1) {
                for(j <- 0 to dimension-1) {
                    cost_matrix(i)(j) = sc.nextDouble()
                }
            }
        }

        val ga = new GA(cost_matrix, 100, 100, 1, 0.2, 0.01, 10, log)
        ga.run()
    }
}
import scala.io.Codec
import java.nio.charset.CodingErrorAction
import scala.io.Source
import java.util.Scanner
import java.io.File

package reader {
    object CSVReader{
        var delimiter = ","

        def read(file: String) : Array[Array[Double]] = {
            implicit val codec = Codec("UTF-8")
            codec.onMalformedInput(CodingErrorAction.REPLACE)
            codec.onUnmappableCharacter(CodingErrorAction.REPLACE)
            val bufferedSource = io.Source.fromFile(file)
            var costMatrix: Array[Array[Double]] = null
            var j: Int = 0
            var k: Int = 0
            var firstLine: Boolean = true
            for (line <- bufferedSource.getLines()) {
                if(!firstLine) {
                    var cols = line.split(delimiter).map(_.trim)
                    // skips first columns
                    cols = cols.slice(1, cols.size)

                    // memory optimization
                    if(costMatrix == null) {
                        costMatrix = Array.ofDim[Double](cols.size, cols.size)
                    }

                    for (cell <- cols) {
                        costMatrix(j)(k) = cell.toDouble
                        k += 1
                    }
                    k = 0
                    j += 1
                }
                else {
                    // skips header
                    firstLine = false
                    delimiter = "" + line(0)
                }
            }
            return(costMatrix)
        }
    }

    object TXTReader {
        def read(file: String) : Array[Array[Double]] = {
            val sc = new Scanner(new File(file))
            val dimension = sc.nextInt()
            var costMatrix = Array.ofDim[Double](dimension, dimension)

            for(i <- 0 to dimension-1) {
                for(j <- 0 to dimension-1) {
                    costMatrix(i)(j) = sc.nextDouble()
                }
            }
            return(costMatrix)
        }
    }
}
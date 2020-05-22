import scala.io.Codec
import java.nio.charset.CodingErrorAction
import scala.io.Source
import java.util.Scanner
import java.io.File

package reader {
    object CSVReader{
        def read(file: String) : Array[Array[Double]] = {
            implicit val codec = Codec("UTF-8")
            codec.onMalformedInput(CodingErrorAction.REPLACE)
            codec.onUnmappableCharacter(CodingErrorAction.REPLACE)
            val bufferedSource = io.Source.fromFile(file)
            val header :: lines = bufferedSource.getLines().toList
            val delimiter = "" + header(0)
            val headerColumns = header.split(delimiter).map(_.trim)
            val costMatrix: Array[Array[Double]] = Array.ofDim[Double](headerColumns.size, headerColumns.size)

            for ((line, rowIndex) <- lines zip (0 until lines.size)) {
                val name :: columns = line.split(delimiter).map(_.trim).toList
                for ((cell, columnIndex) <- columns zip (0 until columns.size)) {
                    costMatrix(rowIndex)(columnIndex) = cell.toDouble
                }
            }
        
            costMatrix
        }
    }

    object TXTReader {
        def read(file: String) : Array[Array[Double]] = {
            val sc = new Scanner(new File(file))
            val dimension = sc.nextInt()
            val costMatrix = Array.ofDim[Double](dimension, dimension)
            for(i <- 0 until dimension) {
                for(j <- 0 until dimension) {
                    costMatrix(i)(j) = sc.nextDouble()
                }
            }
            
            costMatrix
        }
    }
}
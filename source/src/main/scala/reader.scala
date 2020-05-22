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
            val costMatrix: Array[Array[Double]] = Array.ofDim[Double](headerColumns.size - 1, headerColumns.size - 1)

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
            implicit val codec = Codec("UTF-8")
            codec.onMalformedInput(CodingErrorAction.REPLACE)
            codec.onUnmappableCharacter(CodingErrorAction.REPLACE)
            val bufferedSource = io.Source.fromFile(file)
            val header :: lines = bufferedSource.getLines().toList
            val dimension = header.toInt
            val costMatrix = Array.ofDim[Double](dimension, dimension)
            for ((line, rowIndex) <- lines zip (0 until lines.size)) {
                val name :: columns = line.split(" ").map(_.trim).toList.filter(x => x.trim() != "")
                for ((cell, columnIndex) <- columns zip (0 until columns.size)) {
                    costMatrix(rowIndex)(columnIndex) = cell.toDouble
                }
            }
            
            costMatrix
        }
    }
}

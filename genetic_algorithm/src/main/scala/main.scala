import scala.io.Source
import genetic_algorithm.GA
import reader.CSVReader
import reader.TXTReader

object Main {
    def main(args: Array[String]) : Unit = {
        if(args.size < 1) {
            throw new Exception("Wrong number of arguments! Usage: run [cost_matrix_file_name].[csv|txt]")
        }

        val prefix = "../examples/"
        val filename = prefix + args(0)
        var log: Boolean = false

        var cost_matrix: Array[Array[Double]] = null
        if(args(0).endsWith(".csv")) {
            cost_matrix = CSVReader.read(filename)
        }
        else if(args(0).endsWith(".txt")){
            cost_matrix = TXTReader.read(filename)
        } else {
            throw new Exception("Invalid file extension! Usage: run [FILE_NAME].[csv|txt]")
        }

        val ga = new GA(cost_matrix, 1000, 100, 1, 0.2, 0.01, 10, log)
        ga.run()
    }
}
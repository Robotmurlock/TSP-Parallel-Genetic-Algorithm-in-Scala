import scala.io.Source
import genetic_algorithm.GA
import reader.CSVReader
import reader.TXTReader

object Main {
    def main(args: Array[String]) : Unit = {
        val prefix = "src/main/tsp_examples/"
        val filename = prefix + args(0)
        var log: Boolean = false

        var cost_matrix: Array[Array[Double]] = null
        if(args.size > 1 &&  args(1) == "-as-csv") {
            cost_matrix = CSVReader.read(filename)
        }
        else {
            cost_matrix = TXTReader.read(filename)
        }

        val ga = new GA(cost_matrix, 1000, 100, 1, 0.2, 0.01, 10, log)
        ga.run()
    }
}
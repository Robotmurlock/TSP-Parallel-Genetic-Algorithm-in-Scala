import scala.io.Source
import java.util.Scanner
import java.io.File
import genetic_algorithm.GA


object HelloSbt {
    def main(args: Array[String]) = {
        val prefix = "src/main/tsp_examples/"
        val filename = prefix + args(0)

        val sc = new Scanner(new File(filename))

        val dimension = sc.nextInt()
        val cost_matrix = Array.ofDim[Double](dimension, dimension)

        println("Cost matrix: ")
        for(i <- 0 to dimension-1) {
            for(j <- 0 to dimension-1) {
                cost_matrix(i)(j) = sc.nextDouble()
            }
        }

        // Testing file input
        for(i <- 0 to dimension-1) {
            for(j <- 0 to dimension-1) {
                print(cost_matrix(i)(j))
                print(' ')
            }
            println()
        }

        val ga = new GA(cost_matrix, 12, 10, 2, 0.2, 0.01)
        ga.run()
    }
}
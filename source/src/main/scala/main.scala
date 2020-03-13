import scala.io.Source
import java.util.Scanner
import java.io.File


object HelloSbt {
    def main(args: Array[String]) = {
        val prefix = "src/main/tsp_examples/"
        val filename = prefix + args(0)

        val sc = new Scanner(new File(filename))
        
        val n = sc.nextInt()
        val matrix = Array.ofDim[Int](n,n)

        for(i <- 0 to n-1) {
            for(j <- 0 to n-1) {
                matrix(i)(j) = sc.nextInt()
            }
        }

        // Testing file input
        for(i <- 0 to n-1) {
            for(j <- 0 to n-1) {
                print(matrix(i)(j))
                print(' ')
            }
            println()
        }
    }
}
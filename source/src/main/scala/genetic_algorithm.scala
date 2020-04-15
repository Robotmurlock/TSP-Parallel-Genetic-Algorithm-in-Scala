import scala.util.Random

package genetic_algorithm {
    class Chromosome(val size: Int) {

        // #TODO
        def fitness() : Int = {
            return 0
        }

        // #TODO
        override def toString() : String = { 
            return(fitness().toString()); 
        } 
    }

    class GA(val cost_matrix: Array[Array[Double]],
             val population_size: Int, 
             val max_iterations: Int,
             val tournament_size: Int,
             val elitism_ratio: Double, 
             val mutation_rate: Double) {
        
        val random = new Random
        val chromosome_size = cost_matrix.size
        var population: Array[Chromosome] = init_population(chromosome_size)

        def init_population(size: Int) : Array[Chromosome] = {
            var new_population = Array.ofDim[Chromosome](population_size)
            
            for(i <- 0 to population_size-1) {
                new_population(i) = new Chromosome(size)
            }

            return(new_population)
        }

        def select(population: Array[Chromosome]) : Chromosome = {
            // #TODO
            var index = random.nextInt(population_size)
            return(population(index))
        }

        def crossover(p1: Chromosome, p2: Chromosome) : (Chromosome, Chromosome) = {
            // #TODO
            return(p1, p2)
        }

        def mutate(c: Chromosome) : Chromosome = {
            // #TODO
            return(c)
        }

        def best_chromosome(population: Array[Chromosome]) : Chromosome = {
            // $TODO
            return(population(0))
        }

        def run() {
            var population = init_population(population_size)

            for(i <- 0 to max_iterations) {
                var new_population = Array.ofDim[Chromosome](population_size)
                // Elitism (#TODO)

                for (j <- 0 to population_size/2-1) {
                    // Selection (#TODO)
                    var p1 = select(population)
                    var p2 = select(population)

                    // Crossover (#TODO)
                    var (c1, c2) = crossover(p1, p2)

                    // Mutation (#TODO)
                    c1 = mutate(c1)
                    c2 = mutate(c2)

                    new_population(2*j) = c1
                    new_population(2*j+1) = c2
                }

                // Replacement (#TODO)
                population = new_population

                // Algorithm status (#TODO)
                var bc = best_chromosome(population)
                println("Iteration: " + i.toString() + ", fitness: " + bc.toString())
            }
        }
    }
}
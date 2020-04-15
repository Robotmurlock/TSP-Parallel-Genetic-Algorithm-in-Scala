import scala.util.Random

package genetic_algorithm {
    object Global {
        val random = new Random
    }

    class Chromosome(val size: Int, val cost_matrix: Array[Array[Double]]) {

        var content = Array.ofDim[Int](size)
        // Knuth shuffles ~ Random Permutation
        // ref: https://en.wikipedia.org/wiki/Random_permutation

        // Init content: 1 2 3 ...
        for(i <- 0 to size-1) {
            content(i) = i
        }

        // Knuth shuffles:
        for(i <- 0 to size-2) {
            var j = i + Global.random.nextInt(size-i)
            var tmp: Int = content(i)
            content(i) = content(j)
            content(j) = tmp
        }
        
        // returns path as string
        def path() : String = {
            var out = ""
            for(i <- 0 to size-2) {
                out += content(i).toString() + " -> "
            }
            out += content(size-1).toString()
            return(out)
        }

        // calculates fitness using cost matrix
        // #TODO: optimize
        def fitness() : Double = {
            var cost: Double = 0
            for(i <- 0 to size-2) {
                cost += cost_matrix(content(i))(content(i+1))
            }
            cost += cost_matrix(content(size-1))(content(0))
            return(cost)
        }

        override def toString() : String = { 
            return("path: " + path() + ", fitness: " + fitness().toString()); 
        } 
    }

    class GA(val cost_matrix: Array[Array[Double]],
             val population_size: Int, 
             val max_iterations: Int,
             val tournament_size: Int,
             val elitism_ratio: Double, 
             val mutation_rate: Double) {
        
        val chromosome_size = cost_matrix.size

        def init_population(size: Int) : Array[Chromosome] = {
            var new_population = Array.ofDim[Chromosome](population_size)
            
            for(i <- 0 to population_size-1) {
                new_population(i) = new Chromosome(size, cost_matrix)
            }

            return(new_population)
        }

        def select(population: Array[Chromosome]) : Chromosome = {
            // #TODO
//             var length:Int=population.size();
//             if(length<tournament_size)
//                 return null
            
            var index = Global.random.nextInt(population_size)
            return(population(index))
        }

        def crossover(p1: Chromosome, p2: Chromosome) : (Chromosome, Chromosome) = {
            // #TODO
            return(p1, p2)
        }

        def mutate(c: Chromosome) : Chromosome = {
            // #XXX
            var mutate_random:Float=Global.random.nextFloat();
            if(mutate_random<mutation_rate){
                var first_random:Int=Global.random.nextInt(c.size)
                var second_random:Int=Global.random.nextInt(c.size)
                var tmp=c.content(first_random)
                c.content(first_random)=c.content(second_random)
                c.content(second_random)=tmp
            }
            return(c)
        }

        def best_chromosome(population: Array[Chromosome]) : Chromosome = {
            var result: Chromosome = null
            for(i <- 0 to population_size-1) {
                if(result == null || population(i).fitness() < result.fitness()) {
                    result = population(i)
                }
            }
            return(result)
        }

        def run() {
            var population: Array[Chromosome] = init_population(chromosome_size)

            for(i <- 0 to max_iterations-1) {
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
                println("Iteration: " + i.toString() + ", " + bc.toString())
            }
        }
    }
}

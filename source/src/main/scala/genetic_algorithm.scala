import scala.util.Random

package genetic_algorithm {
    object Global {
        val random = new Random
    }

    class Chromosome(val size: Int, val cost_matrix: Array[Array[Double]]) {

        var content = Array.ofDim[Int](size)
        var fit: Double = -1
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

        def copy() : Chromosome = {
            var chrom = new Chromosome(size, cost_matrix)
            for(i <- 0 to size-1) {
                chrom.content(i) = content(i)
            }
            // fitness must be calculated again
            chrom.fit = -1
            return chrom
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
        def fitness() : Double = {
            var cost: Double = 0
            // If fitness was already calculated then there is no need to calculate again
            if(this.fit != -1) {
                cost = this.fit
            }
            else {
                for(i <- 0 to size-2) {
                    cost += cost_matrix(content(i))(content(i+1))
                }
                cost += cost_matrix(content(size-1))(content(0))
                this.fit = cost
            }
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
            // #XXX
            var length:Int=population.length;
            if(length<tournament_size)
                return null
            var random_array=new Array[Chromosome](tournament_size)
            for(i<-0 until tournament_size){
                var r:Int=Global.random.nextInt(tournament_size-1)
                random_array(i)=population(r)
            }
            var min:Double=random_array(0).fitness()
            var index:Int =0 
            for(i<-1 until tournament_size){
                if(random_array(i).fitness()<min){
                    min=random_array(i).fitness
                    index=i
                }
            }
            return(population(index))
        }

        def crossover_next_index(index: Int, size: Int) : Int = {
            var next = index + 1
            if(next == size) {
                next = 0
            }
            return(next)
        }

        def crossover(p1: Chromosome, p2: Chromosome) : (Chromosome, Chromosome) = {
            var c1 = p1.copy()
            var c2 = p2.copy()

            var first_random: Int = Global.random.nextInt(p1.size)
            var second_random: Int = Global.random.nextInt(p1.size)

            if(second_random < first_random) {
                var tmp = first_random
                first_random = second_random
                second_random = tmp
            }

            for(i <- first_random to second_random-1) {
                c1.content(i) = p2.content(i)
                c2.content(i) = p1.content(i)
            }

            var c2_used = p1.content.slice(first_random, second_random).toSet
            var c1_used = p2.content.slice(first_random, second_random).toSet

            var c1_index = second_random
            var c2_index = second_random

            for(i <- second_random to p1.size-1) {
                if(!(c1_used contains p1.content(i))) {
                    c1.content(c1_index) = p1.content(i)
                    c1_index = crossover_next_index(c1_index, p1.size)
                }
                if(!(c2_used contains p2.content(i))) {
                    c2.content(c2_index) = p2.content(i)
                    c2_index = crossover_next_index(c2_index, p1.size)
                }
            }

            for(i <- 0 to second_random-1) {
                if(!(c1_used contains p1.content(i))) {
                    c1.content(c1_index) = p1.content(i)
                    c1_index = crossover_next_index(c1_index, p1.size)
                }
                if(!(c2_used contains p2.content(i))) {
                    c2.content(c2_index) = p2.content(i)
                    c2_index = crossover_next_index(c2_index, p1.size)
                }
            }

            return(c1, c2)
        }

        def mutate(c: Chromosome) : Chromosome = {
            var mutate_random: Float = Global.random.nextFloat();
            var mutated = c.copy()
            if(mutate_random < mutation_rate) {
                var first_random: Int = Global.random.nextInt(mutated.size)
                var second_random: Int = Global.random.nextInt(mutated.size)
                var tmp = mutated.content(first_random)
                mutated.content(first_random) = mutated.content(second_random)
                mutated.content(second_random) = tmp
            }
            return(mutated)
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
                // Elitism (#MAYBE)

                for (j <- 0 to population_size/2-1) {
                    // Selection (#TODO)
                    var p1 = select(population)
                    var p2 = select(population)

                    // Crossover 
                    var (c1, c2) = crossover(p1, p2)

                    // Mutation 
                    c1 = mutate(c1)
                    c2 = mutate(c2)

                    new_population(2*j) = c1
                    new_population(2*j+1) = c2
                }

                // Replacement
                population = new_population

                // Algorithm status 
                var bc = best_chromosome(population)
                println("Iteration: " + i.toString() + ", " + bc.toString())
            }
        }
    }
}

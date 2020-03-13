package genetic_algorithm {
    class Chromosome(val size: Int) {

    }

    class GA(val cost_matrix: Array[Array[Double]],
             val population_size: Int, 
             val max_iterations: Int,
             val tournament_size: Int,
             val elitism_ratio: Double, 
             val mutation_rate: Double) {
        
        val chromosome_size = cost_matrix.size
        var population: Array[Chromosome] = init_population(chromosome_size)

        def init_population(size: Int) : Array[Chromosome] = {
            var new_population = Array.ofDim[Chromosome](population_size)
            
            for(i <- 0 to population_size-1) {
                new_population(i) = new Chromosome(size)
            }

            return(new_population)
        }

    }
}
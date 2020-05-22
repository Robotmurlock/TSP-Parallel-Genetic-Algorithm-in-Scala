import scala.util.Random
import scala.util.Sorting
import java.io._
import java.util.concurrent.atomic.AtomicInteger

package genetic_algorithm {
    object Global {
        val random = new Random
    }

    class Chromosome(val size: Int, val costMatrix: Array[Array[Double]]) {

        var content = Array.ofDim[Int](size)
        var fit: Double = -1
        // Knuth shuffles ~ Random Permutation
        // ref: https://en.wikipedia.org/wiki/Random_permutation

        // Init content: 1 2 3 ...
        for(i <- 0 until size) {
            content(i) = i
        }

        // Knuth shuffles:
        for(i <- 1 to size-2) {
            var j = i + Global.random.nextInt(size-i)
            var tmp: Int = content(i)
            content(i) = content(j)
            content(j) = tmp
        }

        def copy() : Chromosome = {
            var chrom = new Chromosome(size, costMatrix)
            for(i <- 0 until size) {
                chrom.content(i) = content(i)
            }
            // fitness must be calculated again
            chrom.fit = -1
            return chrom
        }
        
        // returns path as string
        def path(delimiter: String = " -> ") : String = {
            var out = ""
            for(i <- 0 to size-2) {
                out += content(i).toString() + delimiter
            }
            out += content(size-1).toString() + delimiter + content(0).toString();
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
                    cost += costMatrix(content(i))(content(i+1))
                }
                cost += costMatrix(content(size-1))(content(0))
                this.fit = cost
            }
            return(cost)
        }

        def normalize() = {
            if(content(0) != 0) {
                var zero_index: Int = 0
                for(i <- 0 until size) {
                    if(content(i) == 0) {
                        zero_index = i
                    }
                }
                content = content.drop(zero_index) ++ content.take(zero_index)
            }
        }

        override def toString() : String = { 
            return("path: " + path() + ", fitness: " + fitness().toString()); 
        } 
    }

    class GA(val costMatrix: Array[Array[Double]],
             val populationSize: Int, 
             val maxIterations: Int,
             var tournamentSize: Int,
             val elitismRatio: Double, 
             val mutationRate: Double,
             val numOfThreads: Int,
             val prefix: String) {
        
        val chromosomeSize = costMatrix.size
        val elitesCnt: Int = (populationSize*elitismRatio).toInt
        var atomicIndex: AtomicInteger = new AtomicInteger(elitesCnt/2)

        if(tournamentSize > populationSize) {
            println("Tournament size(" + tournamentSize.toString() + ") is bigger than population size(" + populationSize.toString() + "). Reducing tournament size to population size.")
            tournamentSize = populationSize
        }

        def compare(a: Chromosome, b: Chromosome) = a.fitness() compare b.fitness()

        def initPopulation(size: Int) : Array[Chromosome] = {
            var newPopulation = Array.ofDim[Chromosome](populationSize)
            
            for(i <- 0 until populationSize) {
                newPopulation(i) = new Chromosome(size, costMatrix)
            }

            return(newPopulation)
        }

        def select(population: Array[Chromosome]) : Chromosome = {
            var random_array = new Array[Chromosome](tournamentSize)
            for(i <- 0 until tournamentSize){
                var r: Int = Global.random.nextInt(populationSize-1)
                random_array(i) = population(r)
            }

            var min: Double = random_array(0).fitness()
            var index: Int = 0 
            for(i <- 1 until tournamentSize){
                if(random_array(i).fitness() < min){
                    min = random_array(i).fitness
                    index = i
                }
            }
            return(population(index))
        }

        def crossoverNextIndex(index: Int, size: Int) : Int = {
            var next = index + 1
            if(next == size) {
                next = 0
            }
            return(next)
        }

        def crossover(p1: Chromosome, p2: Chromosome) : (Chromosome, Chromosome) = {
            var c1 = p1.copy()
            var c2 = p2.copy()

            var firstRandom: Int = Global.random.nextInt(p1.size)
            var secondRandom: Int = Global.random.nextInt(p1.size)

            if(secondRandom < firstRandom) {
                var tmp = firstRandom
                firstRandom = secondRandom
                secondRandom = tmp
            }

            for(i <- firstRandom until secondRandom) {
                c1.content(i) = p2.content(i)
                c2.content(i) = p1.content(i)
            }

            var c2_used = p1.content.slice(firstRandom, secondRandom).toSet
            var c1_used = p2.content.slice(firstRandom, secondRandom).toSet

            var c1_index = secondRandom
            var c2_index = secondRandom

            for(i <- secondRandom until p1.size) {
                if(!(c1_used contains p1.content(i))) {
                    c1.content(c1_index) = p1.content(i)
                    c1_index = crossoverNextIndex(c1_index, p1.size)
                }
                if(!(c2_used contains p2.content(i))) {
                    c2.content(c2_index) = p2.content(i)
                    c2_index = crossoverNextIndex(c2_index, p1.size)
                }
            }

            for(i <- 0 until secondRandom) {
                if(!(c1_used contains p1.content(i))) {
                    c1.content(c1_index) = p1.content(i)
                    c1_index = crossoverNextIndex(c1_index, p1.size)
                }
                if(!(c2_used contains p2.content(i))) {
                    c2.content(c2_index) = p2.content(i)
                    c2_index = crossoverNextIndex(c2_index, p1.size)
                }
            }

            return(c1, c2)
        }

        def mutate(c: Chromosome) : Chromosome = {
            var mutate_random: Float = Global.random.nextFloat();
            var mutated = c.copy()
            if(mutate_random < mutationRate) {
                var firstRandom: Int = 1 + Global.random.nextInt(mutated.size-1)
                var secondRandom: Int = 1 + Global.random.nextInt(mutated.size-1)
                var tmp = mutated.content(firstRandom)
                mutated.content(firstRandom) = mutated.content(secondRandom)
                mutated.content(secondRandom) = tmp
            }
            return(mutated)
        }

        def bestChromosome(population: Array[Chromosome]) : Chromosome = {
            var result: Chromosome = null
            for(i <- 0 until populationSize) {
                if(result == null || population(i).fitness() < result.fitness()) {
                    result = population(i)
                }
            }
            return(result)
        }

        def resultsToCsv(data: Array[Chromosome]) = {
            val filename = prefix + "result.csv"
            val file = new File(filename)
            val writer = new BufferedWriter(new FileWriter(file))
            writer.write("iteration, fitness, path\n")
            (0 until maxIterations).foreach(
                i => writer.write(i.toString + ", " + data(i).fitness().toString() + ", \"" + data(i).path(";") + "\"\n"))
            writer.close()
        }

        class EvolutionRunnable(population: Array[Chromosome], newPopulation: Array[Chromosome]) extends Runnable {
            def run() : Unit = {

                var index: Int = atomicIndex.getAndIncrement()

                while(index < populationSize/2) {
                    // Selection 
                    var p1 = select(population)
                    var p2 = select(population)
                    // Crossover 
                    var (c1, c2) = crossover(p1, p2)
                    // Mutation 
                    c1 = mutate(c1)
                    c2 = mutate(c2)
                    // Normalizing values
                    c1.normalize()
                    c2.normalize()
                    // adding mutated chromosomes to new population
                    newPopulation(2*index) = c1
                    newPopulation(2*index+1) = c2
                    // updating global index
                    index = atomicIndex.getAndIncrement()
                }
            }
        }

        def run() : Unit = {
            var population: Array[Chromosome] = initPopulation(chromosomeSize)
            var data: Array[Chromosome] = Array.ofDim[Chromosome](maxIterations)

            for(i <- 0 until maxIterations) {
                var newPopulation = Array.ofDim[Chromosome](populationSize)
                atomicIndex.set(elitesCnt/2)

                // Elitism
                scala.util.Sorting.quickSort(population)(compare)
                (0 until elitesCnt).foreach(j => newPopulation(j) = population(j))

                // Creating new generation using threads
                var threads: Array[Thread] = 
                    (for(j <- 0 until numOfThreads) 
                        yield (new Thread(new EvolutionRunnable(population, newPopulation)))
                    ).toArray
                threads.foreach(t => t.start())
                threads.foreach(t => t.join())

                // Replacement
                population = newPopulation

                // Algorithm status 
                var bc = bestChromosome(population)
                data(i) = bc.copy()
            }

            resultsToCsv(data)
        }
    }
}

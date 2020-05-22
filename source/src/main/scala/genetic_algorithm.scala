import scala.util.Random
import scala.util.Sorting
import java.io._
import java.util.concurrent.atomic.AtomicInteger

package genetic_algorithm {
    object Global {
        val random = new Random
    }

    class Chromosome(val size: Int, val costMatrix: Array[Array[Double]]) {

        val content = Array.ofDim[Int](size)
        // Knuth shuffles ~ Random Permutation
        // ref: https://en.wikipedia.org/wiki/Random_permutation

        // Init content: 1 2 3 ...
        for(i <- 0 until size) {
            content(i) = i
        }

        // Knuth shuffles:
        for(i <- 1 to size-2) {
            val j = i + Global.random.nextInt(size-i)
            val tmp: Int = content(i)
            content(i) = content(j)
            content(j) = tmp
        }

        def copy() : Chromosome = {
            val chrom = new Chromosome(size, costMatrix)
            val copiedContent = content.clone()
            (0 until size).foreach(index => chrom.content(index) = copiedContent(index))
            return(chrom)
        }
        
        // returns path as string
        def path(delimiter: String = " -> ") : String = {
            (0 until size-1).foldLeft("") {
                (acc, index) => acc + content(index).toString() + delimiter
            } + content(size-1).toString() + delimiter + content(0).toString();
        }

        // calculates fitness using cost matrix
        def fitness() : Double = {
            (0 until size).foldLeft(0.0) {
                (acc, index) => 
                    if (index != size-1) (acc + costMatrix(content(index))(content(index+1)))
                    else (acc + costMatrix(content(size-1))(content(0)))
            }
        }

        def normalize() : Unit = {
            if(content(0) != 0) {
                for(i <- 0 until size) {
                    if(content(i) == 0) {
                        val normalizedContent = content.drop(i) ++ content.take(i)
                        (0 until size).foreach(index => content(index) = normalizedContent(index))
                    }
                }
            }
        }

        override def toString() : String = { 
            "path: " + path() + ", fitness: " + fitness().toString() 
        } 
    }

    class GA(val costMatrix: Array[Array[Double]],
             val populationSize: Int, 
             val maxIterations: Int,
             val _tournamentSize: Int,
             val elitismRatio: Double, 
             val mutationRate: Double,
             val numOfThreads: Int,
             val prefix: String) {
        
        val chromosomeSize = costMatrix.size
        val elitesCnt: Int = (populationSize*elitismRatio).toInt
        val atomicIndex: AtomicInteger = new AtomicInteger(elitesCnt/2)
        val tournamentSize = if (_tournamentSize > populationSize) {
            println("Tournament size(" + _tournamentSize.toString() + ") is bigger than population size(" + populationSize.toString() + "). Reducing tournament size to population size.")
            populationSize
        } else _tournamentSize

        def compare(a: Chromosome, b: Chromosome) = a.fitness() compare b.fitness()

        def initPopulation(size: Int) : Array[Chromosome] = {
            val newPopulation = Array.ofDim[Chromosome](populationSize)
            (0 until populationSize).foreach(index => newPopulation(index) = new Chromosome(size, costMatrix))
            return(newPopulation)
        }

        def select(population: Array[Chromosome]) : Chromosome = {
            val randomArray = new Array[Chromosome](tournamentSize)
            for(i <- 0 until tournamentSize){
                val r: Int = Global.random.nextInt(populationSize-1)
                randomArray(i) = population(r)
            }
            val bestChromosomeIndex = ((randomArray zip (1 until tournamentSize)).fold((randomArray(0), 0)) {
                (acc, x) => if (x._1.fitness() < acc._1.fitness()) x else acc
            })._2
            population(bestChromosomeIndex)
        }

        def crossoverNextIndex(index: Int, size: Int) : Int = {
            if (index+1 == size) (0) else (index+1)
        }

        def crossover(p1: Chromosome, p2: Chromosome) : (Chromosome, Chromosome) = {
            val c1 = p1.copy()
            val c2 = p2.copy()

            val firstRandom: Int = Global.random.nextInt(p1.size)
            val secondRandom: Int = firstRandom + Global.random.nextInt(p1.size-firstRandom)

            for(i <- firstRandom until secondRandom) {
                c1.content(i) = p2.content(i)
                c2.content(i) = p1.content(i)
            }

            val c2Used = p1.content.slice(firstRandom, secondRandom).toSet
            val c1Used = p2.content.slice(firstRandom, secondRandom).toSet

            var c1Index = secondRandom
            var c2Index = secondRandom

            for(i <- (secondRandom until p1.size) ++ (0 until secondRandom)) {
                if(!(c1Used contains p1.content(i))) {
                    c1.content(c1Index) = p1.content(i)
                    c1Index = crossoverNextIndex(c1Index, p1.size)
                }
                if(!(c2Used contains p2.content(i))) {
                    c2.content(c2Index) = p2.content(i)
                    c2Index = crossoverNextIndex(c2Index, p1.size)
                }
            }
            return(c1, c2)
        }

        def mutate(c: Chromosome) : Chromosome = {
            val mutateRandom: Float = Global.random.nextFloat();
            val mutated = c.copy()
            if(mutateRandom < mutationRate) {
                val firstRandom: Int = 1 + Global.random.nextInt(mutated.size-1)
                val secondRandom: Int = 1 + Global.random.nextInt(mutated.size-1)
                val tmp = mutated.content(firstRandom)
                mutated.content(firstRandom) = mutated.content(secondRandom)
                mutated.content(secondRandom) = tmp
            }
            return(mutated)
        }

        def bestChromosome(population: Array[Chromosome]) : Chromosome = {
            population.fold(population(0)) {
                (acc, x) => if(x.fitness() < acc.fitness) x else acc
            }
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
            def next(index: Int) : Unit = {
                if(index < populationSize/2) {
                    // Selection 
                    val p1 = select(population)
                    val p2 = select(population)
                    // Crossover 
                    val (c1, c2) = crossover(p1, p2)
                    // Mutation (with normalization)
                    val c1Mutated = mutate(c1)
                    val c2Mutated = mutate(c2)
                    // normalization
                    c1Mutated.normalize()
                    c2Mutated.normalize()
                    // adding mutated chromosomes to new population
                    newPopulation(2*index) = c1Mutated
                    newPopulation(2*index+1) = c2Mutated
                    // updating global index
                    next(atomicIndex.getAndIncrement())
                }
            }

            def run() : Unit = {
                next(atomicIndex.getAndIncrement())
            }
        }

        def run() : Unit = {
            val population: Array[Chromosome] = initPopulation(chromosomeSize)
            val data: Array[Chromosome] = Array.ofDim[Chromosome](maxIterations)

            for(i <- 0 until maxIterations) {
                val newPopulation = Array.ofDim[Chromosome](populationSize)
                atomicIndex.set(elitesCnt/2)

                // Elitism
                scala.util.Sorting.quickSort(population)(compare)
                (0 until elitesCnt).foreach(j => newPopulation(j) = population(j))

                // Creating new generation using threads
                val threads: Array[Thread] = 
                    (for(j <- 0 until numOfThreads) 
                        yield (new Thread(new EvolutionRunnable(population, newPopulation)))
                    ).toArray
                threads.foreach(t => t.start())
                threads.foreach(t => t.join())

                // Replacement
                (0 until populationSize).foreach(index => population(i) = newPopulation(i))

                // Algorithm status 
                data(i) = bestChromosome(population).copy()
            }

            resultsToCsv(data)
        }
    }
}

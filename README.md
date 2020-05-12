# TSP-Parallel-Genetic-Algorithm-in-Scala
Implementation of Parallel Genetic Algorithm in Scala for solving Travelling Salesman Problem with optional animation using scalafx.

# Requirement:
`sbt`, `https://www.scala-sbt.org/`

# To run genetic algorithm with animation:
`position in sources`<br>
`sbt`<br>
`compile`<br>
`run`<br>

# Parameters:
Parameters for genetic algorithm can be changed in `parameter.csv` file:<br>
`population_size`<br> 
`max_iterations`<br>
`tournament_size`<br>
`elitism_rate`<br>
`mutation_rate`<br>
`num_of_threads`<br>

# Custom TSP problem:
By making your own TSP cost matrix as `.csv` file or as `.txt` file and changing `cost_matrix` parameter in `parameter.csv` file you can run genetic algorithm to solve your TSP problem.

# Making your own animation:
By supplying your own image and cities location alongside TSP problem (previous section) you can make your own animation
by changing `cities` and `image` parameter in `parameter.csv` file.

# Result:
![result](https://raw.githubusercontent.com/Robotmurlock/TSP-Parallel-Genetic-Algorithm-in-Scala/master/docs/tsp.mp4)

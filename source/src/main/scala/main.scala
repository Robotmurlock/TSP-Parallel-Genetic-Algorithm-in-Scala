import scala.io.StdIn
import scala.io.Source
import genetic_algorithm.GA
import reader.CSVReader
import reader.TXTReader

import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.scene.Scene
import scalafx.scene.layout.BorderPane
import scalafx.scene.canvas.Canvas
import scala.util.Random
import scalafx.animation.AnimationTimer
import scalafx.scene.paint.Color
import scala.io.Source
import scala.collection.immutable.List
import scala.collection.mutable.ArrayBuffer
import scalafx.scene.image._
import scalafx.scene.control._
import scalafx.scene.input._
import scalafx.event.ActionEvent

object Main extends JFXApp{
    val parameterSource = io.Source.fromFile("../parameter.csv")
    var parameterSourceDelimiter = ","
    var first_line: Boolean = true;
    var header: Array[String] = null

    var cost_matrix_path: String = "Srbija2.csv"
    var cities_path: String = "Cities2.csv"
    var image_path: String = "srbija.jpg"
    var population_size: Int = 1000
    var max_iterations: Int = 100
    var tournament_size: Int = 20
    var elitism_rate: Double = 0.2
    var mutation_rate: Double = 0.01
    var num_of_threads: Int = 10

    for (line <- parameterSource.getLines().slice(0, 2)) {
        var cols = line.split(parameterSourceDelimiter).map(_.trim)
        if(!first_line) {
            for(i <- 0 until cols.size) {
                header(i) match {
                    case "cost_matrix"     => cost_matrix_path = cols(i).replace("\"", "")
                    case "cities"          => cities_path      = cols(i).replace("\"", "")
                    case "image"           => image_path       = cols(i).replace("\"", "")
                    case "population_size" => population_size  = cols(i).toInt
                    case "max_iterations"  => max_iterations   = cols(i).toInt
                    case "tournament_size" => tournament_size  = cols(i).toInt
                    case "elitism_rate"    => elitism_rate     = cols(i).toDouble
                    case "mutation_rate"   => mutation_rate    = cols(i).toDouble
                    case "num_of_threads"  => num_of_threads   = cols(i).toInt
                    case _                 => println("Warning: Unknown header!")
                }
            }
        }
        else {
            first_line = false
            header = cols
        }
    }

    val prefix: String = "../examples/"
    val filename = prefix + cost_matrix_path

    var cost_matrix: Array[Array[Double]] = null
    if(cost_matrix_path.endsWith(".csv")) {
        cost_matrix = CSVReader.read(filename)
    }
    else if(cost_matrix_path.endsWith(".txt")){
        cost_matrix = TXTReader.read(filename)
    } else {
        throw new Exception("Invalid file extension! Usage: run [FILE_NAME].[csv|txt]")
    }

    val ga = new GA(cost_matrix, 
                    population_size, 
                    max_iterations, 
                    tournament_size, 
                    elitism_rate, 
                    mutation_rate, 
                    num_of_threads, 
                    prefix)
    ga.run()


    var Salesman = new vect(0, 0)
    //Find a city with given index from the input file
    def findCity(Cities: ArrayBuffer[vect], index: Int) : vect = {
        for(c <- Cities ){
            if(c.index == index){
            var found = new vect(c.x, c.y)  
            found.named(c.name)
            found.setIndex(index)
            return found
            }
        }
        return new vect(-1, -1)
    }


	
    stage = new JFXApp.PrimaryStage {
        title = "Animation"
        
        //Loading the serbia map
        val img = new Image("file:" + prefix + image_path)
        
        //default image dimensions
        val imageWidth = 370
        val imageHeight = 570
        //setting parameters for the scene and canvas to match the image size
        val sceneWidth = img.width.toInt
        val sceneHeight = img.height.toInt
        scene = new Scene(sceneWidth, sceneHeight) {
            
            val view = new ImageView(img)
            
            
            val  border = new BorderPane
            val canvas = new Canvas(sceneWidth,sceneHeight)
            val gc = canvas.graphicsContext2D
            border.center = canvas
            
            content = List(view, border)
            
            //finding coordinates of cities by clicking on the map
            //onMousePressed = (e:MouseEvent) => {
            //  println(e.x, e.y)
            //}
            
            //time measurement for each generation
            var timeStart = System.nanoTime
            var duration = (System.nanoTime - timeStart)/1e9d
            
            
            //points/cities displayed on the map
            var Points = ArrayBuffer[vect]()
            
            val fileOrder = prefix + "result.csv"
            
            //reading the order of the salesman tour for every generation
            val iterLine = io.Source.fromFile(fileOrder).getLines.drop(1)
            
            
            val generationStep = ga.max_iterations/10
            
            var Order = ArrayBuffer[Int]()
            
            //loading order of 1 generation at a time
            if(iterLine.hasNext) {
                val cols = iterLine.next().split(",").map(_.trim)
                
                val travelOrder = cols(2).replace("\"", "").trim().split(";")
                
                println("Generacija " + cols(0))
                
                for(City <- travelOrder){
                    Order += City.toInt
                }
                iterLine.drop(generationStep)
            }
            var Visited = 0
            
            
            //loading each city to display
            val filename = prefix + cities_path
            for (line <- Source.fromFile(filename).getLines.drop(1)) {
                val coordinates = line.split(',').map(_.trim)
                Points += new vect((coordinates(1).toDouble/imageWidth)*sceneWidth, (coordinates(2).toDouble/imageHeight)*sceneHeight)
                Points.last.named(coordinates(0))
                Points.last.setIndex(coordinates(3).toInt)
            }
            
            

            val SalesmanSpeed = 5
            
            val SalesmanMov = new vect(0, 0)


			//indicator for the cities help determianting the color on the map
            var visitedCities = ArrayBuffer[Boolean]()
            
            for(i <- 0 to Points.size - 1) {
                visitedCities += false
            }
            
            
            
            var currentCity = findCity(Points, Order(0))
            //setting salesman starting position in generation 0
            Salesman.x = currentCity.x
            Salesman.y = currentCity.y
            
            
            currentCity = findCity(Points, Order(Visited))
            //setting salesman movement vector based on the order 
            SalesmanMov.x = currentCity.x - Salesman.x
            SalesmanMov.y = currentCity.y - Salesman.y
            SalesmanMov.normalize()
            
            
            val timer = AnimationTimer { time =>
                gc.drawImage(img, 0, 0)
                
                //transition to the display of next generation
                if(Visited == Order.size){
                    Order = ArrayBuffer[Int]()
                
                    
                    Visited = 0
					//showing the time of the previous generation
					var duration = (System.nanoTime - timeStart)/1e9d
					println("Potrebno vreme za obilazak u sekundama: " + duration)
					
					//leaveing room for the preview of the finished generation
					Thread.sleep(1000)
					//starting timer for another generation
					timeStart = System.nanoTime
					
					//reading the next generation
                    if(iterLine.hasNext) {
                        val cols = iterLine.next().split(",").map(_.trim)
                        println("---------------------------")
                        println("Generacija  " + cols(0))
                    
                        val travelOrder = cols(2).replace("\"", "").trim().split(";")
                        for(City <- travelOrder){
                            Order += City.toInt
                        }
                        //reseting location  of the salesman
                        currentCity = findCity(Points, Order(0))
                        Salesman.x = currentCity.x
						Salesman.y = currentCity.y
                        
                        currentCity = findCity(Points, Order(Visited))

                        SalesmanMov.x = currentCity.x - Salesman.x
                        SalesmanMov.y = currentCity.y - Salesman.y
                        SalesmanMov.normalize()
                        
                        
                        //reseting visited used to determine the color of the city
                        for(i <- 0 to Points.size - 1){
                            visitedCities(i) = false
                        }
                    }else
						System.exit(0)
				    iterLine.drop(generationStep) //skipping generations
                }
                
                //check if the salesman has come to the city and advancing to the next city if he had visited the previous
                currentCity = findCity(Points, Order(Visited))
                if(Visited < Order.size && (Salesman.x - currentCity.x).abs <= SalesmanSpeed && (Salesman.y - currentCity.y).abs <= SalesmanSpeed) {
                    visitedCities(Order(Visited)) = true
                    println("Posetio sam " + currentCity.name)
                    Visited += 1
                    if(Visited != Order.size) {
                        currentCity = findCity(Points, Order(Visited))
                
                        SalesmanMov.x = currentCity.x - Salesman.x
                        SalesmanMov.y = currentCity.y - Salesman.y
                        SalesmanMov.normalize()
                    }
                }
                
                //coloring the cities basted on the cities salesman has visited
                for(i <- 0 to Points.size - 1) {
                    currentCity = findCity(Points, i)
                    if(visitedCities(i) == true)
                        gc.fill = Color.Black
                    else
                        gc.fill = Color.Yellow
                    gc.fillOval(currentCity.x, currentCity.y, 20, 20)
                }
                
                //movement of the salesman
                gc.fill = Color.Cyan
                Salesman.x = Salesman.x + SalesmanMov.x*SalesmanSpeed
                Salesman.y = Salesman.y + SalesmanMov.y*SalesmanSpeed
                gc.fillRect(Salesman.x, Salesman.y, 10, 10)
            }
            timer.start()
        }
    }
}

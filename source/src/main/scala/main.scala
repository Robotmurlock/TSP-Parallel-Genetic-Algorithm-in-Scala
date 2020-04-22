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
        
        val img = new Image("file:" + prefix + image_path)
        
        val sceneWidth = img.width.toInt
        val sceneHeight = img.height.toInt
        scene = new Scene(sceneWidth, sceneHeight) {
            
            val view = new ImageView(img)
            
            
            
            val  border = new BorderPane
            val canvas = new Canvas(sceneWidth,sceneHeight)
            val gc = canvas.graphicsContext2D
            border.center = canvas
            
            
            content = List(view, border)
            
            //onMousePressed = (e:MouseEvent) => {
            //  println(e.x, e.y)
            //}
            
            var timeStart = System.nanoTime
            var duration = (System.nanoTime - timeStart)/1e9d
            
            var Points = ArrayBuffer[vect]()
            
            val fileOrder = prefix + "result.csv"
            
            val iterLine = io.Source.fromFile(fileOrder).getLines.drop(1)
            
            val generationStep = ga.max_iterations/10
            
            var Order = ArrayBuffer[Int]()
            
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
            
            
            val filename = prefix + cities_path
            for (line <- Source.fromFile(filename).getLines.drop(1)) {
                val coordinates = line.split(',').map(_.trim)
                Points += new vect((coordinates(1).toDouble/370)*sceneWidth, (coordinates(2).toDouble/570)*sceneHeight)
                Points.last.named(coordinates(0))
                Points.last.setIndex(coordinates(3).toInt)
            }
            
            


            val SalesmanSpeed = 5
            
            val SalesmanMov = new vect(0, 0)

            var visitedCities = ArrayBuffer[Boolean]()
            
            for(i <- 0 to Points.size - 1) {
                visitedCities += false
            }
            
            
            var startingIndex = 0
            
            var currentCity = findCity(Points, startingIndex)
            
            Salesman.x = Points(Order(0)).x
            Salesman.y = Points(Order(0)).y
            
            println(Points(Order(0)).x)
            
            currentCity = findCity(Points, Order(Visited))
            
            SalesmanMov.x = currentCity.x - Salesman.x
            SalesmanMov.y = currentCity.y - Salesman.y
            SalesmanMov.normalize()
            
            
            val timer = AnimationTimer { time =>
                gc.drawImage(img, 0, 0)
                
                
                if(Visited == Order.size){
                    Order = ArrayBuffer[Int]()
                
                    currentCity = findCity(Points, startingIndex)
            
                    
                    Visited = 0
                
					var duration = (System.nanoTime - timeStart)/1e9d
					println("Potrebno vreme za obilazak u sekundama: " + duration)
					
                
					Thread.sleep(1000)
					
					timeStart = System.nanoTime
					
                    if(iterLine.hasNext) {
                        val cols = iterLine.next().split(",").map(_.trim)
                        println("---------------------------")
                        println("Generacija  " + cols(0))
                    
                        val travelOrder = cols(2).replace("\"", "").trim().split(";")
                        for(City <- travelOrder){
                            Order += City.toInt
                        }
                        currentCity = findCity(Points, Order(Visited))
                        
                        Salesman.x = Points(Order(0)).x
						Salesman.y = Points(Order(0)).y
                
                        SalesmanMov.x = currentCity.x - Salesman.x
                        SalesmanMov.y = currentCity.y - Salesman.y
                        SalesmanMov.normalize()
                        
                        for(i <- 0 to Points.size - 1){
                            visitedCities(i) = false
                        }
                    }else
						System.exit(0)
				    iterLine.drop(generationStep)
                }
                
                
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
                
                
                for(i <- 0 to Points.size - 1) {
                    currentCity = findCity(Points, i)
                    if(visitedCities(i) == true)
                        gc.fill = Color.Black
                    else
                        gc.fill = Color.Yellow
                    gc.fillOval(currentCity.x, currentCity.y, 20, 20)
                }
                
                
                gc.fill = Color.Cyan
                Salesman.x = Salesman.x + SalesmanMov.x*SalesmanSpeed
                Salesman.y = Salesman.y + SalesmanMov.y*SalesmanSpeed
                gc.fillRect(Salesman.x, Salesman.y, 10, 10)
            }
            timer.start()
        }
    }
}

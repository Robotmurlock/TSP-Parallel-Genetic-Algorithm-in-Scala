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
    val parameterSourceDelimiter = ","
    var firstLine: Boolean = true;
    var header: Array[String] = null

    var costMatrixPath: String = "Srbija2.csv"
    var citiesPath: String = "Cities2.csv"
    var imagePath: String = "srbija.jpg"
    var populationSize: Int = 1000
    var maxIterations: Int = 100
    var tournamentSize: Int = 20
    var elitismRate: Double = 0.2
    var mutationRate: Double = 0.01
    var numOfThreads: Int = 10

    for (line <- parameterSource.getLines().slice(0, 2)) {
        var cols = line.split(parameterSourceDelimiter).map(_.trim)
        if(!firstLine) {
            for(i <- 0 until cols.size) {
                header(i) match {
                    case "costMatrix"      => costMatrixPath  = cols(i).replace("\"", "")
                    case "cities"          => citiesPath      = cols(i).replace("\"", "")
                    case "image"           => imagePath       = cols(i).replace("\"", "")
                    case "populationSize"  => populationSize  = cols(i).toInt
                    case "maxIterations"   => maxIterations   = cols(i).toInt
                    case "tournamentSize"  => tournamentSize  = cols(i).toInt
                    case "elitismRate"     => elitismRate     = cols(i).toDouble
                    case "mutationRate"    => mutationRate    = cols(i).toDouble
                    case "numOfThreads"    => numOfThreads    = cols(i).toInt
                    case _                 => println("Warning: Unknown header!")
                }
            }
        }
        else {
            firstLine = false
            header = cols
        }
    }

    val prefix: String = "../examples/"
    val filename = prefix + costMatrixPath

    var costMatrix: Array[Array[Double]] = null
    if(costMatrixPath.endsWith(".csv")) {
        costMatrix = CSVReader.read(filename)
    }
    else if(costMatrixPath.endsWith(".txt")){
        costMatrix = TXTReader.read(filename)
    } else {
        throw new Exception("Invalid file extension! Usage: run [FILE_NAME].[csv|txt]")
    }

    val ga = new GA(costMatrix, 
                    populationSize, 
                    maxIterations, 
                    tournamentSize, 
                    elitismRate, 
                    mutationRate, 
                    numOfThreads, 
                    prefix)
    ga.run()


    var salesman = new vect(0, 0)
    //Find a city with given index from the input file
    def findCity(cities: ArrayBuffer[vect], index: Int) : vect = {
        for(c <- cities ){
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
        val img = new Image("file:" + prefix + imagePath)
        
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
            var points = ArrayBuffer[vect]()
            
            val fileorder = prefix + "result.csv"
            
            //reading the order of the salesman tour for every generation
            val iterLine = io.Source.fromFile(fileorder).getLines.drop(1)
            
            
            val generationStep = ga.maxIterations/10
            
            var order = ArrayBuffer[Int]()
            
            //loading order of 1 generation at a time
            if(iterLine.hasNext) {
                val cols = iterLine.next().split(",").map(_.trim)
                
                val travelOrder = cols(2).replace("\"", "").trim().split(";")
                
                println("Generacija " + cols(0))
                
                for(City <- travelOrder){
                    order += City.toInt
                }
                iterLine.drop(generationStep)
            }
            var visited = 0
            
            
            //loading each city to display
            val filename = prefix + citiesPath
            for (line <- Source.fromFile(filename).getLines.drop(1)) {
                val coordinates = line.split(',').map(_.trim)
                points += new vect((coordinates(1).toDouble/imageWidth)*sceneWidth, (coordinates(2).toDouble/imageHeight)*sceneHeight)
                points.last.named(coordinates(0))
                points.last.setIndex(coordinates(3).toInt)
            }
            
            

            val salesmanSpeed = 5
            
            val salesmanMov = new vect(0, 0)


			//indicator for the cities help determianting the color on the map
            var visitedCities = ArrayBuffer[Boolean]()
            
            for(i <- 0 to points.size - 1) {
                visitedCities += false
            }
            
            
            
            var currentCity = findCity(points, order(0))
            //setting salesman starting position in generation 0
            salesman.x = currentCity.x
            salesman.y = currentCity.y
            
            
            currentCity = findCity(points, order(visited))
            //setting salesman movement vector based on the order 
            salesmanMov.x = currentCity.x - salesman.x
            salesmanMov.y = currentCity.y - salesman.y
            salesmanMov.normalize()
            
            
            val timer = AnimationTimer { time =>
                gc.drawImage(img, 0, 0)
                
                //transition to the display of next generation
                if(visited == order.size){
                    order = ArrayBuffer[Int]()
                
                    
                    visited = 0
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
                            order += City.toInt
                        }
                        //reseting location  of the salesman
                        currentCity = findCity(points, order(0))
                        salesman.x = currentCity.x
						salesman.y = currentCity.y
                        
                        currentCity = findCity(points, order(visited))

                        salesmanMov.x = currentCity.x - salesman.x
                        salesmanMov.y = currentCity.y - salesman.y
                        salesmanMov.normalize()
                        
                        
                        //reseting visited used to determine the color of the city
                        for(i <- 0 to points.size - 1){
                            visitedCities(i) = false
                        }
                    }else
						System.exit(0)
				    iterLine.drop(generationStep) //skipping generations
                }
                
                //check if the salesman has come to the city and advancing to the next city if he had visited the previous
                currentCity = findCity(points, order(visited))
                if(visited < order.size && (salesman.x - currentCity.x).abs <= salesmanSpeed && (salesman.y - currentCity.y).abs <= salesmanSpeed) {
                    visitedCities(order(visited)) = true
                    println("Posetio sam " + currentCity.name)
                    visited += 1
                    if(visited != order.size) {
                        currentCity = findCity(points, order(visited))
                
                        salesmanMov.x = currentCity.x - salesman.x
                        salesmanMov.y = currentCity.y - salesman.y
                        salesmanMov.normalize()
                    }
                }
                
                //coloring the cities basted on the cities salesman has visited
                for(i <- 0 to points.size - 1) {
                    currentCity = findCity(points, i)
                    if(visitedCities(i) == true)
                        gc.fill = Color.Black
                    else
                        gc.fill = Color.Yellow
                    gc.fillOval(currentCity.x, currentCity.y, 20, 20)
                }
                
                //movement of the salesman
                gc.fill = Color.Cyan
                salesman.x = salesman.x + salesmanMov.x*salesmanSpeed
                salesman.y = salesman.y + salesmanMov.y*salesmanSpeed
                gc.fillRect(salesman.x, salesman.y, 10, 10)
            }
            timer.start()
        }
    }
}

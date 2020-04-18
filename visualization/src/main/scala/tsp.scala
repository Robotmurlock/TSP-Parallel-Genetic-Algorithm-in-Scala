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

object tsp extends JFXApp{

 
  private var Salesman = new vect(0, 0)
  
  stage = new JFXApp.PrimaryStage{
    title = "Animation"
    
    val sceneWidth = 370
    val sceneHeight = 570
    scene = new Scene(sceneWidth, sceneHeight){
      
      val img = new Image("file:src/images/srbija.jpg", sceneWidth, sceneHeight, true, true)
      
      
      val view = new ImageView(img)
      
      
      
      val  border = new BorderPane
      val canvas = new Canvas(sceneWidth,sceneHeight)
      val gc = canvas.graphicsContext2D
      border.center = canvas
      
      
      content = List(view, border)
      
      onMousePressed = (e:MouseEvent) => {
        println(e.x, e.y)
      }
      
      
      var Points = ArrayBuffer[vect]()
      
      val fileOrder = "src/result.csv"
      
      val iterLine = io.Source.fromFile(fileOrder).getLines.drop(1)
      
      
      
      var Order = ArrayBuffer[Int]()
      
      if(iterLine.hasNext){
        val cols = iterLine.next().split(",").map(_.trim)
        
        val travelOrder = cols(2).replace("\"", "").trim().split(";")
        
        
        println("Generacija " + cols(0))
        
        for(City <- travelOrder){
          Order += City.toInt
        }
      }
      var Visited = 0
      
      
      val filename = "src/1.txt"
      for (line <- Source.fromFile(filename).getLines) {
        val coordinates = line.split(' ')
        Points += new vect(coordinates(0).toInt, coordinates(1).toInt)
        Points.last.named(coordinates(2))
      }
      
      
      println("Test 1")

      val SalesmanSpeed = 10
      
      val SalesmanMov = new vect(0, 0)

      var visitedCities = ArrayBuffer[Boolean]()
      
      for(i <- 0 to Points.size - 1){
        visitedCities += false
      }
      
      SalesmanMov.x = Points(Order(Visited)).x - Salesman.x
      SalesmanMov.y = Points(Order(Visited)).y - Salesman.y
      SalesmanMov.normalize()
      
      
      val timer = AnimationTimer { time =>
        gc.drawImage(img, 0, 0)
        
        
        
        if(Visited == Order.size){
          Order = ArrayBuffer[Int]()
          Salesman.x = 0
          Salesman.y = 0
          Visited = 0
          
          if(iterLine.hasNext){
            val cols = iterLine.next().split(",").map(_.trim)
            println("---------------------------")
            println("Generacija  " + cols(0))
        
            val travelOrder = cols(2).replace("\"", "").trim().split(";")
            for(City <- travelOrder){
              Order += City.toInt
            }
            SalesmanMov.x = Points(Order(Visited)).x - Salesman.x
            SalesmanMov.y = Points(Order(Visited)).y - Salesman.y
            SalesmanMov.normalize()
            
            for(i <- 0 to Points.size - 1){
              visitedCities(i) = false
            }
          }
        }
        
        if(Visited < Order.size && (Salesman.x - Points(Order(Visited)).x).abs <= SalesmanSpeed && (Salesman.y - Points(Order(Visited)).y).abs <= SalesmanSpeed){
          visitedCities(Order(Visited)) = true
          println("Posetio sam " + Points(Order(Visited)).name)
          Visited += 1
          if(Visited != Order.size){
            SalesmanMov.x = Points(Order(Visited)).x - Salesman.x
            SalesmanMov.y = Points(Order(Visited)).y - Salesman.y
            SalesmanMov.normalize()
          }
        }
        
        
        gc.fill = Color.Cyan
        Salesman.x = Salesman.x + SalesmanMov.x*SalesmanSpeed
        Salesman.y = Salesman.y + SalesmanMov.y*SalesmanSpeed
        gc.fillRect(Salesman.x, Salesman.y, 10, 10)
        
        
        for(i <- 0 to Points.size - 1){
          if(visitedCities(i) == true)
            gc.fill = Color.Pink
          else
            gc.fill = Color.Yellow
          gc.fillOval(Points(i).x, Points(i).y, 20, 20)
        }
      }
      timer.start()
    }
  }
}

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
import scalafx.scene.image.ImageView


object tsp extends JFXApp{

 
  private var Salesman = new vect(0, 0)
  
  stage = new JFXApp.PrimaryStage{
    title = "Animation"
    scene = new Scene(600, 600){
      val  border = new BorderPane
      val canvas = new Canvas(600,600)
      val gc = canvas.graphicsContext2D
      border.center = canvas
      root = border
      
      
      var Points = ArrayBuffer[vect]()
      
      val fileOrder = "src/2.txt"
      
      val iterLine = Source.fromFile(fileOrder).getLines
      
      
       
      var Order = ArrayBuffer[Int]()
       
      val travelOrder = iterLine.next().split(' ')
      
      for(City <- travelOrder){
        Order += City.toInt
      }
      
      var Visited = 0
      
      val filename = "src/1.txt"
      for (line <- Source.fromFile(filename).getLines) {
        val coordinates = line.split(' ')
        Points += new vect(coordinates(0).toInt, coordinates(1).toInt)
      }
      

      val SalesmanSpeed = 2
      
      val SalesmanMov = new vect(0, 0)

      var visitedCities = ArrayBuffer[Boolean]()
      
      for(i <- 0 to Points.size - 1){
        visitedCities += false
      }
      
      SalesmanMov.x = Points(Order(Visited)).x - Salesman.x
      SalesmanMov.y = Points(Order(Visited)).y - Salesman.y
      SalesmanMov.normalize()
      
      
      val timer = AnimationTimer { time =>
        gc.fill = Color.White
        gc.fillRect(0,0, canvas.width(), canvas.height())
        
        if(Visited == Order.size){
          Order = ArrayBuffer[Int]()
          Salesman.x = 0
          Salesman.y = 0
          Visited = 0
          
          if(iterLine.hasNext){
            val travelOrder1 = iterLine.next().split(' ')
        
            
            for(City <- travelOrder1){
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
        
        if(Visited < Order.size && (Salesman.x - Points(Order(Visited)).x).abs <= 1 && (Salesman.y - Points(Order(Visited)).y).abs <= 1){
          visitedCities(Order(Visited)) = true
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
            gc.fill = Color.Green
          else
            gc.fill = Color.Red
          gc.fillOval(Points(i).x, Points(i).y, 20, 20)
        }
      }
      timer.start()
    }
  }
}
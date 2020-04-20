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
  
  
  
  stage = new JFXApp.PrimaryStage{
    title = "Animation"
    
    val img = new Image("file:src/images/srbija.jpg")
    
    val sceneWidth = img.width.toInt
    val sceneHeight = img.height.toInt
    scene = new Scene(sceneWidth, sceneHeight){
      
      
      
      val view = new ImageView(img)
      
      
      
      val  border = new BorderPane
      val canvas = new Canvas(sceneWidth,sceneHeight)
      val gc = canvas.graphicsContext2D
      border.center = canvas
      
      
      content = List(view, border)
      
      //onMousePressed = (e:MouseEvent) => {
      //  println(e.x, e.y)
      //}
      
      
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
      
      
      val filename = "src/cities.csv"
      for (line <- Source.fromFile(filename).getLines.drop(1)) {
        val coordinates = line.split(',').map(_.trim)
        Points += new vect((coordinates(1).toDouble/370)*sceneWidth, (coordinates(2).toDouble/570)*sceneHeight)
        Points.last.named(coordinates(0))
        Points.last.setIndex(coordinates(3).toInt)
      }
      
      


      val SalesmanSpeed = 2
      
      val SalesmanMov = new vect(0, 0)

      var visitedCities = ArrayBuffer[Boolean]()
      
      for(i <- 0 to Points.size - 1){
        visitedCities += false
      }
      
      val startingIndex = 0
      
      var currentCity = findCity(Points, startingIndex)
      
      Salesman.x = currentCity.x
      Salesman.y = currentCity.y
      
      currentCity = findCity(Points, Order(Visited))
      
      SalesmanMov.x = currentCity.x - Salesman.x
      SalesmanMov.y = currentCity.y - Salesman.y
      SalesmanMov.normalize()
      
      
      val timer = AnimationTimer { time =>
        gc.drawImage(img, 0, 0)
        
        
        if(Visited == Order.size){
          Order = ArrayBuffer[Int]()
          
		  currentCity = findCity(Points, startingIndex)
      
		  Salesman.x = currentCity.x
		  Salesman.y = currentCity.y
          Visited = 0
          
          if(iterLine.hasNext){
            val cols = iterLine.next().split(",").map(_.trim)
            println("---------------------------")
            println("Generacija  " + cols(0))
        
            val travelOrder = cols(2).replace("\"", "").trim().split(";")
            for(City <- travelOrder){
              Order += City.toInt
            }
            currentCity = findCity(Points, Order(Visited))
      
		    SalesmanMov.x = currentCity.x - Salesman.x
		    SalesmanMov.y = currentCity.y - Salesman.y
            SalesmanMov.normalize()
            
            for(i <- 0 to Points.size - 1){
              visitedCities(i) = false
            }
          }
          Thread.sleep(1000)
        }
        
        
        currentCity = findCity(Points, Order(Visited))
        if(Visited < Order.size && (Salesman.x - currentCity.x).abs <= SalesmanSpeed && (Salesman.y - currentCity.y).abs <= SalesmanSpeed){
          visitedCities(Order(Visited)) = true
          println("Posetio sam " + currentCity.name)
          Visited += 1
          if(Visited != Order.size){
            currentCity = findCity(Points, Order(Visited))
      
		    SalesmanMov.x = currentCity.x - Salesman.x
		    SalesmanMov.y = currentCity.y - Salesman.y
            SalesmanMov.normalize()
          }
        }
        
        
        for(i <- 0 to Points.size - 1){
		  currentCity = findCity(Points, i)
          if(visitedCities(i) == true)
            gc.fill = Color.Pink
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

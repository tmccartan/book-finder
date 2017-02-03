import java.io.{File, BufferedWriter, FileWriter}

import play.api.libs.json.Json
import play.api.libs.ws.ning.NingWSClient
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

object BookFinder extends App {

  val wsClient = NingWSClient()
  val apiKey = io.Source.fromFile("api.config").getLines().next()

  def findBooks() = {
    val bufferedSource = io.Source.fromFile("books.csv")
    val lineItr = bufferedSource.getLines.drop(1)


    val res = Future.sequence(lineItr.take(1).map {
      lines => {
          val strArr = lines.split(",")
          val fname = strArr(1)
          val lname = strArr(0)
          val title = strArr(2).replaceAll("\"", "")
          getBookDetails(title, fname, lname)
        }
    })
    val file = new File("updated-books.csv")
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write("First Name, Last Name, Title, Genre, Rating, Link \n")

    res.map( strings => {
      strings.foreach( line => {
        println(line)
        bw.write(line + "\n")
      })
      wsClient.close()
      bw.close()
    })
  }

  def getBookDetails (title:String, firstName: String, lastName:String): Future[String] = {

    val author = firstName + lastName
    val stringQuery = s"inauthor:$author+intitle:$title"
    val volumeIdFuture = wsClient.url("https://www.googleapis.com/books/v1/volumes").withQueryString("q" -> stringQuery, "key" -> apiKey).get()
      .map { wsResponse =>
        wsResponse.status match  {
          case 200 =>
            val json = Json.parse(wsResponse.body)
            ((json \ "items") (0) \ "id").toOption match {
              case Some(value) => Right(value.as[String])
              case None => Left("unknown")
            }
          case _ =>
            Left("unknown")
        }
      }

    val detailString = volumeIdFuture.flatMap {
      case Right(volumeId) =>
      wsClient.url(s"https://www.googleapis.com/books/v1/volumes/$volumeId").withQueryString("key" -> apiKey).get.map {
        volumeResponse => {
          volumeResponse.status match  {
            case 200 => {
              val volJson = Json.parse(volumeResponse.body)
              val infoJson = (volJson \ "volumeInfo").get
              val rating = (infoJson \ "averageRating").toOption.fold(0.0)(_.as[Double])
              val infoLine = (infoJson \ "infoLink").get.as[String]
              val mainCategory = (infoJson \ "categories")(0).as[String]
              val result = s"$firstName, $lastName, $title, $mainCategory, $rating, $infoLine"
              result
            }
            case code =>
              println("Error " + volumeResponse)
              s"$firstName, $lastName, $title"
          }

        }
      }
      case Left(u) =>
        Future.successful(s"$firstName, $lastName, $title")
    }
    detailString
  }

  findBooks()
}

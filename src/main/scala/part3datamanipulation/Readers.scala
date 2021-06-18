package part3datamanipulation

object Readers {

  /*
    - configuration file => initial data structure
    - a DB layer
    - an HTTP layer
    - a business logic layer
   */

  case class Configuration(dbUsername: String,
                           dbPassword: String,
                           host: String,
                           port: Int,
                           nThreads: Int,
                           emailReplyTo: String)

  case class DbConnection(username: String, password: String) {
    def getOrderStatus(orderId: Long): String =
      "dispatched" // select * from the db table and return the status of orderId

    def getLastOrderId(username: String): Long = 54643
  }

  case class HttpService(host: String, port: Int) {
    def start(): Unit = println("server started")
  }

  // bootstrap
  val config =
    Configuration("daniel", "rockthejvm1!", "localhost", 1234, 8, "daniel")

  // cats Reader
  import cats.data.Reader
  val dbReader: Reader[Configuration, DbConnection] = Reader(
    conf => DbConnection(conf.dbUsername, conf.dbPassword)
  )
  val dbConn = dbReader.run(config)

  // Reader[I, O]
  val danielsOrderStatusReader: Reader[Configuration, String] =
    dbReader.map(dbConn => dbConn.getOrderStatus(55))
  val danielOrderStatus = danielsOrderStatusReader.run(config)

  def getLastOrderStatus(username: String): String = {
    val usersOrderFor = for {
      lastOrderId <- dbReader.map(_.getLastOrderId(username))
      orderStatus <- dbReader.map(_.getOrderStatus(lastOrderId))
    } yield orderStatus
    usersOrderFor.run(config)
  }

  /*
    Pattern
    1. you create the initial data structure
    2. you create a reader which specifies how that data structure will be manipulation later
    3. you can then map & flatMap the reader to produce derived information
    4. when you need the final piece of information, you call run on the reader with the initial data structure
   */

  case class EmailService(emailReplyTo: String) {
    def sendEmail(address: String, contents: String) =
      s"From $emailReplyTo; to: $address >>> $contents"
  }

  // TODO
  def emailUser(username: String, userEmail: String) = {
    // fetch the status of their last order
    // email them with the Email service: "Your last order has the status: (status)"
    val emailServiceReader: Reader[Configuration, EmailService] = Reader(
      conf => EmailService(conf.emailReplyTo)
    )
    val emailReader: Reader[Configuration, String] = for {
      lastOrderId <- dbReader.map(_.getLastOrderId(username))
      orderStatus <- dbReader.map(_.getOrderStatus(lastOrderId))
      emailService <- emailServiceReader
    } yield
      emailService.sendEmail(
        userEmail,
        s"You last order the status: $orderStatus"
      )
    emailReader.run(config)
  }

  // TODO 2: what programming pattern do Readers remind you of?
  // Dependencies Injection!

  def main(args: Array[String]): Unit = {
    println(getLastOrderStatus("daniel"))
    println(emailUser("daniel", "daniel@rockthejvm.com"))
  }
}
import com.github.decyg.Neo4JConnection
import org.scalatest.FlatSpec

import scala.io.StdIn

class BoltSpec extends FlatSpec{

  "A given Neo4jConnection instance" should "be able to open a connection" in {
    new Neo4JConnection().connect()

    StdIn.readLine()

  }
}

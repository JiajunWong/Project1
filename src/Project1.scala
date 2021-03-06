import akka.actor.Actor
import akka.actor.Props
import akka.actor.ActorSystem
import scala.math._

class SlaveActor extends Actor {

  def receive = {
    case (origin: Long, begin: Long, tasklongth: Long, numOfSequence: Long) =>
      for (i <- begin to (begin + tasklongth - 1)) {
        val ai = origin + (i - 1) * (i + numOfSequence) * numOfSequence
        val root = round(sqrt(ai))
        if (ai == root * root) {
          sender ! (root, i, numOfSequence)
        }
      }
      sender ! 1l
  }
}

class LeaderActor extends Actor {
  private var numSubtasks = 4
  private var waitToExit = 0l;

  def receive = {
    case (n: Long) => {
      waitToExit = waitToExit + n
      if (waitToExit == numSubtasks) {
        context.stop(self)
        println("actor are all stoped!")
        sys.exit()
      }
    }

    case (n: Long, k: Long) => {
      val origin = k * (k + 1) * (2 * k + 1) / 6
      
      if(n < 10) {
        numSubtasks = 1
      }

      for (j <- 1 to numSubtasks) {
        (context.actorOf(Props[SlaveActor], ""+j)) ! (origin, 1 + (j - 1) * n / numSubtasks, n / numSubtasks, k)
      }
    }

    case (root: Long, exp: Long, numOfSequence: Long) => {
      println("" + root + "^2 = " + (for (i <- exp to (exp + numOfSequence - 1)) yield "" + i + "^2").mkString("+"))
    }
  }
}

object Project1 {

  def main(args: Array[String]) {
    val beginning = if (args.length > 0) args(0).toInt else 3l
    val sequence = if (args.length > 1) args(1).toInt else 2l

    val system = ActorSystem("system")
    val leader = system.actorOf(Props[LeaderActor], "master")
    leader ! (beginning, sequence)

  }
}
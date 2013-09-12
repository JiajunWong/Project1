/**
 * Copyright (C) 2009-2012 Typesafe Inc. <http://www.typesafe.com>
 */
//package project1

import akka.actor.Actor
import akka.actor.ActorRef
import akka.actor.Props
import akka.routing.RoundRobinRouter
import akka.actor.ActorSystem
 
//import akka.actor._
//import akka.routing.RoundRobinRouter
//import scala.concurrent.duration.Duration
//import scala.concurrent.duration._
//import javax.xml.datatype.Duration
 
object project1 extends App {
 
  calculate(nrOfWorkers = 4, nrOfElements = 24, nrOfMessages = 40)
 
  sealed trait PiMessage
  case object Calculate extends PiMessage
  case class Work(start: Int, nrOfElements: Int) extends PiMessage
  case class Result(value: Double) extends PiMessage
  //case class PiApproximation(pi: Double, duration: Duration)
 
  class Worker extends Actor {
 
   // calculate ...
    def calculatethesquare(start: Int, nrOfElements: Int) {
        var acc=0
 
      for (i <- start until start+nrOfElements){
          acc += i*i         
      }
      //  sender ! Result(start)
         var squareroot =0.0
          squareroot= Math.sqrt(acc)
          
      if ((squareroot- squareroot.toInt) == 0)  {
           sender ! Result(start)
        }
     
     
    }
    def receive = {
      case Work(start, nrOfElements) =>
      calculatethesquare(start: Int, nrOfElements: Int)

    } 
  }
  

  
 
  class Master(nrOfWorkers: Int, nrOfMessages: Int, nrOfElements: Int)
    extends Actor {
    
    val workerRouter = context.actorOf(
      Props[Worker].withRouter(RoundRobinRouter(nrOfWorkers)), name = "workerRouter")
 
    def receive = {
      case Calculate =>for (i <- 1 until nrOfMessages+1) workerRouter ! Work(i, nrOfElements)
      case Result(record) => println(record)
         
    }
 
  }
 
 /* class Listener extends Actor {
    def receive = {
      case output =>  println("hello from %s".format(record))
        context.system.shutdown()
    }
  }*/
 
 
  def calculate(nrOfWorkers: Int, nrOfElements: Int, nrOfMessages: Int) {
   
    val system = ActorSystem("PiSystem")
 
    // create the result listener, which will print the result and shutdown the system
   // val listener = system.actorOf(Props[Listener], name = "listener")
 
    // create the master
    val master = system.actorOf(Props(new Master(
      nrOfWorkers, nrOfMessages, nrOfElements)),
      name = "master")
 
    // start the calculation
    master ! Calculate
 
  }
}
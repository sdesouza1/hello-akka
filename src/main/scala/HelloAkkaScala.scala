import akka.actor.{ ActorRef, ActorSystem, Props, Actor, Inbox }
import scala.concurrent.duration._
import java.util.Scanner

case class number(num:Integer)
case class operation(op:Char)
case class Equation(num1:Integer,operator:Char,num2:Integer)
case class Result(num1:Integer,operator:Char,num2:Integer,result:Integer)



class Evaluator extends Actor{
    
    def receive ={
        case Equation(n1,o,n2)=>o match{
            case '+'=>{
              sender!Result(n1,'+',n2,n1+n2)
              context.stop(self)
            }
            case '-'=>{
              sender!Result(n1,'-',n2,n1-n2)
              context.stop(self)
            }
            case '*'=>{
              sender!Result(n1,'*',n2,n1*n2)
              context.stop(self)
            }
            case '%'=>{
              sender!Result(n1,'%',n2,n1%n2)
              context.stop(self)
            }
        } 
    }
}

class Receptionist extends Actor{
    val system = ActorSystem("calculator")
    var count=0
    var num1=0
    var num2=0
    var operator='+'
    var flag1=false
    var flag2=false
    var opflag=false
    val valid="+-%*"
    def receive={
        case number(n) =>{
            if(!flag1) {
                num1=n
                flag1=true
            }
            else if(!flag2) {
                num2=n
                flag2=true
            }
            if(flag1&&flag2&&opflag) {
                system.actorOf(Props[Evaluator],"evaluator"+count.toString())!Equation(num1,operator,num2)
                count+=1
                flag1=false
                flag2=false
                opflag=false
            }
        }
        case operation(op)=>{
            if((!opflag)&&(valid contains op)) {
                operator=op
                opflag=true
                if(flag1&&flag2&&opflag){
                  system.actorOf(Props[Evaluator],"evaluator"+count.toString())!Equation(num1,operator,num2)
                  count+=1
                  flag1=false
                  flag2=false
                  opflag=false
                } 
            }
        }
        case Result(one,op,two,result)=>println(one.toString()+op+two.toString()+"="+result.toString())
        
    }
}

object HelloAkkaScala extends App {

  val system = ActorSystem("calculator")

  val receptionist = system.actorOf(Props[Receptionist], "receptionist")

  // Create an "actor-in-a-box"
  val inbox = Inbox.create(system)
  def isNumeric(in:String):Boolean=in.forall(_.isDigit)


 val scan:Scanner=new Scanner(System.in)
  
 while(true){
   println("say stuff")
   var x=scan.next();
   if(isNumeric(x.trim())) inbox.send(receptionist,number(x.trim().toInt))
   else if(x.trim().length==1) inbox.send(receptionist,operation(x.trim().charAt(0)))//needs to change to get input from html stuff
   
 }
}


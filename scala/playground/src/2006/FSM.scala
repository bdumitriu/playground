trait FiniteStateMachine {
  type T;
  type S <: State;
  type F <: FSM;

  trait State {
    def runState(fsm: F): unit;
    def getNext(x: T): S;
  }

  abstract class FSM(ss: S, endState: S)
    requires F {
    protected var curState = ss;

    def readInput: T;

    def run = {
      while (curState != endState) {
        val x = readInput;
        curState = curState.getNext(x);
        curState.runState(this);
      }
    }
  }
}

object MyFSM extends FiniteStateMachine {
  type T = char;
  type S = PrintState;
  type F = CharFSM;

  abstract class PrintState extends State {
    val msg: String;

    def runState(fsm: CharFSM) = {
      Console.println(msg +
        " after reading character " +
        fsm.input.charAt(fsm.ipos-1));
    }
  }

  class CharFSM(ss: PrintState
              , fs: PrintState
              , i: String)
    extends FSM(ss, fs) {
    val input: String = i; 
    private var pos: int = 0;

    def readInput: char = {
      if (pos == input.length)
        pos = 0;
      pos = pos+1;
      input.charAt(pos-1);
    }

    def ipos = pos;
  }
}

object Test {
  import MyFSM._;

  val s1: PrintState = new PrintState {
    val msg = "In state 1";
    def getNext(x: char): PrintState = x match {
      case 'a' => s2
      case 'b' => s3;
    }
  }

  val s2 = new PrintState {
    val msg = "In state 2";
    def getNext(x: char): PrintState = x match {
      case 'a' => s1;
      case 'b' => this
      case 'c' => s3;
    }
  }

  val s3 = new PrintState {
    val msg = "In state 3";
    def getNext(x: char): PrintState = this;
  }

  val fsm = new CharFSM(s1, s3, "aaabbaabcd");

  def main(args: Array[String]): unit = {
    fsm.run;
  }
}


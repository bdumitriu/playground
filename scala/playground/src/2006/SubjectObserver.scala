abstract class SubjectObserver {
  type S <: Subject;
  type O <: Observer;

  abstract class Subject requires S {
    private var observers: List[O] = List();

    def subscribe(obs: O) = {
      observers = obs :: observers;
    }

    def publish = {
      for (val obs <- observers) {
        obs.notify(this);
      }
    }
  }

  trait Observer {
    def notify(sub: S): unit
  }
}

object SensorReader extends SubjectObserver {
  type S = Sensor;
  type O = Display;

  abstract class Sensor extends Subject {
    val label: String;
    var value: double = 0.0;

    def changeValue(v: double) = {
      value = v;
      publish;
    }
  }

  class Display extends Observer {
    def println(s: String) = {
      Console.println(s);
    }

    def notify(sub: Sensor) = {
      println(sub.label + " has value " + sub.value);
    }
  }
}

object Test {
  import SensorReader._;
  val s1 = new Sensor { val label = "sensor1" }
  val s2 = new Sensor { val label = "sensor2" }
  def main(args: Array[String]) = {
    val d1 = new Display;
    val d2 = new Display;
    s1.subscribe(d1);
    s1.subscribe(d2);
    s2.subscribe(d1);
    s1.changeValue(2);
    s2.changeValue(3);
  }
}


abstract class SubjectObserver {
  abstract class Subject {
    private var observers: List[Observer] = List();

    def subscribe(obs: Observer) = {
      observers = obs :: observers;
    }

    def publish = {
      for (val obs <- observers) {
        obs.notify(this);
      }
    }
  }

  trait Observer {
    def notify(sub: Subject): unit
  }
}

object SensorReader extends SubjectObserver {
  class Sensor(l: String) extends Subject {
    val label: String = l;
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

    override def notify(sub: Sensor) = {
      println(sub.label + " has value " + sub.value);
    }
  }
}

object SO {
  import SensorReader._;
  val s1 = new Sensor("sensor 1");
  val s2 = new Sensor("sensor 2");
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


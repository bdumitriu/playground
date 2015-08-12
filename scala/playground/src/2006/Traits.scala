trait Logger {
  def log(message: String): unit;
}

trait Debugger extends Logger {
  def debug(message: String, active: boolean) = {
    if (active) { log(message); }
  }
}

class FileLogger(path: String) extends Logger {
  import java.io._;
  val f = new FileWriter(new File(path));

  def log(message: String) = {
    f.write(message + "\n");
  }

  def close = {
    f.close;
  }
}

object Test {
  def main(args: Array[String]): unit = {
    class FileDebugger extends FileLogger(args(0))
                       with Debugger;
    var dbg = new FileDebugger;
    dbg.debug("some message", true);
    dbg.close;
  }
}

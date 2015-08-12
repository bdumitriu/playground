trait PrettyPrintable {
  def pp: String;
}

abstract class Buffer {
  type T;
  val capacity: int; 
  protected var buff = new Array[T](capacity);
/*
  def prepend(elem: T): unit = ...
  def append(elem: T): unit = ...
  def remove(pos: int): T = ...
  def clear: unit = ...
*/
}

abstract class DisplayingBuffer
  extends Buffer with PrettyPrintable {
  type T <: PrettyPrintable;

  def pp: String = {
    var result = "";
    for (val elem <- buff) {
      result = result + elem.pp + "\n";
    }
    result;
  }
}


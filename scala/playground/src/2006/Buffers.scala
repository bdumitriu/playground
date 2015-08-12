object Buffers {

  trait Buffer {
    type T;
    val element: T;
  }

  trait SeqBuffer extends Buffer {
    type U;
    type T <: Seq[U];
    def length = element.length;
  }

  trait IntSeqBuffer extends SeqBuffer {
    type U = Int;
  }

  def main(args: Array[String]): Unit = {
    def newIntSeqBuffer(elem1: Int, elem2: Int): IntSeqBuffer = 
      new IntSeqBuffer {
        type T = List[U];
        val element = List(elem1, elem2);
      }

    val buf = newIntSeqBuffer(1, 2);
    Console.println("length = " + buf.length);
    Console.println("content = " + buf.element);
  }
}


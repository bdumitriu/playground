object Test {
 trait MySet[T] {
    def add(x: T): MySet[T];
    def contains(x: T): boolean;
  }

 class MyList[T](h: T) {
   var head: T = h;
   var tail: MyList[T] = null;

   def prepend(x: T): MyList[T] = {
     tail = new MyList[T](head);
     head = x;
   }

   def isEmpty: boolean = false;
 }

  implicit def list2set[T](xs: MyList[T]): MySet[T] = {
    new MySet[T] {
      def add(x: T): MySet[T] = {
        xs prepend x;
      }
      def contains(x: T): boolean = {
        !xs.isEmpty && ((xs.head == x) ||
            (xs.tail contains x))
      }
    }
  }

  def addToSet[T](elem: T, set: MySet[T]) = {
    set.add(elem);
  }

 def main(args: Array[String]): unit = {
    var l = new MyList('a');
    addToSet('b', l);
    Console.println(l.tail.head);
  }
}


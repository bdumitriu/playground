object Quicksort {
  def sort(xs: Array[int]): unit = {
    def swap(i: int, j: int) = {
      val t = xs(i)
      xs(i) = xs(j)
      xs(j) = t
    }
    def sort1(l: int, r: int): unit = {
      val pivot = xs((l+r)/2)
      var i = l
      var j = r
      while (i <= j) {
        while (xs(i) < pivot) {
          i = i + 1
        }
        while (xs(j) > pivot) {
          j = j - 1
        }
        if (i <= j) {
          swap(i, j)
          i = i + 1
          j = j - 1
        }
      }
      if (l < j) {
        sort1(l, j)
      }
      if (j < r) {
        sort1(i, r)
      }
    }
    sort1(0, xs.length - 1)
  }

  def functionalSort(xs: Array[int]): Array[int] = {
    if (xs.length <= 1) {
      xs
    }
    else {
      val pivot = xs(xs.length / 2)
      Array.concat(
        functionalSort(xs filter (pivot >)),
                       xs filter (pivot ==),
        functionalSort(xs filter (pivot <)))
    }
  }
}

object RunQuicksort extends Object with Application {
  var a = Array(1,5,4,3,2,9,6,7,8)
  a = Quicksort.functionalSort(a)
  Console.print("Sorted array: ")
  for (val x <- a.subArray(0, a.length - 1)) yield Console.print("" + x + ",")
  Console.println("" + a(a.length - 1))
}

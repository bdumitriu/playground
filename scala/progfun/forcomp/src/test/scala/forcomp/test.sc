package forcomp

import Anagrams._

object test {
  sentenceAnagrams(List("Linux", "rulez"))        //> res0: List[forcomp.Anagrams.Sentence] = List(List(Zulu, Lin, Rex), List(Zulu,
                                                  //|  nil, Rex), List(Zulu, Rex, Lin), List(Zulu, Rex, nil), List(null, Uzi, Rex),
                                                  //|  List(null, Rex, Uzi), List(Uzi, null, Rex), List(Uzi, Rex, null), List(Lin, 
                                                  //| Zulu, Rex), List(nil, Zulu, Rex), List(Lin, Rex, Zulu), List(nil, Rex, Zulu),
                                                  //|  List(Linux, rulez), List(Rex, Zulu, Lin), List(Rex, Zulu, nil), List(Rex, nu
                                                  //| ll, Uzi), List(Rex, Uzi, null), List(Rex, Lin, Zulu), List(Rex, nil, Zulu), L
                                                  //| ist(rulez, Linux))

  val o = sentenceOccurrences(List("Linux", "rulez"))
                                                  //> o  : forcomp.Anagrams.Occurrences = List((e,1), (i,1), (l,2), (n,1), (r,1), 
                                                  //| (u,2), (x,1), (z,1))
//  combinations(o)


  val s = subtract(o, List(('a',1), ('s',1), ('y',1)))
                                                  //> java.util.NoSuchElementException: key not found: a
                                                  //| 	at scala.collection.MapLike$class.default(MapLike.scala:225)
                                                  //| 	at scala.collection.immutable.HashMap.default(HashMap.scala:38)
                                                  //| 	at scala.collection.MapLike$class.apply(MapLike.scala:135)
                                                  //| 	at scala.collection.immutable.HashMap.apply(HashMap.scala:38)
                                                  //| 	at forcomp.Anagrams$.adjustMapEntry(Anagrams.scala:111)
                                                  //| 	at forcomp.Anagrams$$anonfun$subtract$1.apply(Anagrams.scala:107)
                                                  //| Output exceeds cutoff limit. 

// [ ["men"], ["me", "n"], ["n", "me"] ]
//  dictionaryByOccurrences(List(('a',1), ('s',1), ('y',1)))

// [ ["say", "men"], ["say", "me", "n"], ["say", "n", "me"] ]
}
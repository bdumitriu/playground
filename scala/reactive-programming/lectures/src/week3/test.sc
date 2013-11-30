import week3.{NetworkStack, AdventureGame}

object ag extends AdventureGame

object ns extends NetworkStack
val result = ns.getResult

println(new String(result))


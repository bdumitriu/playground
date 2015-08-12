import scala.util.parsing.Parsers;

object ExpressionOps {
  type Env = (String => int)

  abstract class Tree
  case class Sum(l: Tree, r: Tree) extends Tree
  case class Var(n: String) extends Tree
  case class Const(v: int) extends Tree

  class ExpressionParsers extends Parsers {
    type inputType = String

    def letter = new Parser[Tree] {
      def apply(in: inputType): Result = {
        if (in.length() > 0 && Character.isLetter(in.charAt(0))) {
          return Some(Pair(Var(in.charAt(0).toString()), in.substring(1)))
	}
        else {
          return None
        }
      }
    }
/*
    def digit = new Parser[Tree] {
      def apply(in: inputType): Result = {
        if (in.length() > 0 && Character.isDigit(in.charAt(0))) {
          return Some(Pair())
      }
    }
*/
    def expr: Parser[Tree] = letter
  }

  def eval(t: Tree, env: Env): int = t match {
    case Sum(l, r) => eval(l, env) + eval(r, env)
    case Var(n)    => env(n)
    case Const(v)  => v
  }

  def derive(t: Tree, v: String): Tree = t match {
    case Sum(l, r)          => Sum(derive(l, v), derive(r, v))
    case Var(n) if (n == v) => Const(1)
    case _                  => Const(0)
  }

  def bottomup(t: Tree, f: Tree => Tree): Tree = t match {
    case Sum(l, r) => f(Sum(bottomup(l, f), bottomup(r, f)))
    case tr        => f(tr)
  }

  def simplify(t: Tree): Tree = bottomup(t, t => t match {
    case Sum(Const(0), r)        => r
    case Sum(l, Const(0))        => l
    case Sum(Const(m), Const(n)) => Const(m+n)
    case tr                      => tr
  })

  def main(args: Array[String]): unit = {
    var exp: Tree = Sum(Sum(Var("x"), Var("x")),Sum(Const(7),Var("y")))
//    var p = new ExpressionParsers
//    var exp: Tree = p.expr.apply("x") match {
//                      case Some(Pair(t, "")) => t
//                      case None              => throw new Error("couldn't parse")
//                    }
    var env: Env = {case "x" => 5 case "y" => 7}
    Console.println("Expression: " + exp)
    Console.println("Evaluation with x=5, y=7: " + eval(exp, env))
    Console.println("Derivative relative to x:\n " + derive(exp, "x"))
    Console.println("Derivative relative to y:\n " + derive(exp, "y"))
    Console.println("Derivative relative to x:\n " + simplify(derive(exp, "x")))
    Console.println("Derivative relative to y:\n " + simplify(derive(exp, "y")))
  }
}

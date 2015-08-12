trait BoolLang {
  type E <: BoolExp;
  trait BoolExp {
    def eval: boolean;
  }
  class Val(v: boolean) extends BoolExp {
    val value = v;
    def eval = { value; }
  }
}

/*
object Test extends BoolLang with Application {
  type E = BoolExp;
  val e = new Val(true);
  Console.println(e.eval);
}
*/

trait BLAndOr extends BoolLang {
  class And(l: E, r: E) extends BoolExp {
    val lexp = l;
    val rexp = r;
    def eval = { lexp.eval && rexp.eval; }
  }
  class Or(l: E, r: E) extends BoolExp {
    val lexp = l;
    val rexp = r;
    def eval = { lexp.eval || rexp.eval; }
  }
}

trait BLNot extends BoolLang {
  class Not(e: E) extends BoolExp {
    val exp = e;
    def eval = { ! exp.eval; }
  }
}

trait BLAndOrNot extends BLAndOr with BLNot;

/*
object Test extends BLAndOrNot with Application {
  type E = BoolExp;
  val e = new And(
            new Or(new Val(true), new Val(false))
          , new Not(new Val(true))
          );
  Console.println(e.eval);
}
*/

trait BLShow extends BoolLang with Application {
  type E <: BoolExp;
  trait BoolExp extends super.BoolExp {
    def show: String;
  }
  class Val(v: boolean) extends super.Val(v) with BoolExp {
    def show = { value.toString; }
  }
}

/*
object Test extends BLShow {
  def main(args: Array[String]) = {
    type E = BoolExp;
    val e = new Val(false);
    Console.println(e.show);
  }
}
*/

trait BLShowAndOrNot extends BLAndOrNot with BLShow {
  class And(l: E, r: E) extends super.And(l, r) with BoolExp {
    def show = { "(" + lexp.show + " && " + rexp.show + ")"; }
  }
  class Or(l: E, r: E) extends super.Or(l, r) with BoolExp {
    def show = { lexp.show + " || " + rexp.show; }
  }
  class Not(e: E) extends super.Not(e) with BoolExp {
    def show = { "~(" + exp.eval  + ")"; }
  }
}

/*
object Test extends BLShowAndOrNot with Application {
  type E = BoolExp;
  val e = new And(
            new Or(new Val(true), new Val(false))
          , new Not(new Val(true))
          );
  Console.println(e.show + " = " + e.eval);
}
*/

trait BLSwitchAndOrNot extends BLAndOrNot {
  type E <: BoolExp;
  trait BoolExp extends super.BoolExp {
    def switchArgs: E;
  }
  def Val(v: boolean): E;
  def And(l: E, r: E): E;
  def Or(l:E, r: E): E;
  def Not(e: E): E;
  class Val(v: boolean) extends super.Val(v) with BoolExp {
    def switchArgs = { Val(v); }
  }
  class And(l: E, r: E) extends super.And(l, r) with BoolExp {
    def switchArgs = { And(r.switchArgs, l.switchArgs); }
  }
  class Or(l: E, r: E) extends super.Or(l, r) with BoolExp {
    def switchArgs = { Or(r.switchArgs, l.switchArgs); }
  }
  class Not(e: E) extends super.Not(e) with BoolExp {
    def switchArgs = { Not(e.switchArgs); }
  }
}

/*
object Test extends BLSwitchAndOrNot with Application {
  type E = BoolExp;
  def Val(v: boolean) = { new Val(v); }
  def And(l: E, r: E) = { new And(l, r); }
  def Or(l: E, r: E) = { new Or(l, r); }
  def Not(e: E) = { new Not(e); }
  val e = And(
            Or(Val(true), Val(false))
          , Not(Val(true))
          );
  Console.println(e.switchArgs.eval);
}
*/

trait BLShowSwitchAndOrNot extends BLShowAndOrNot with BLSwitchAndOrNot {
  type E <: BoolExp;
  trait BoolExp extends super[BLShowAndOrNot].BoolExp
                   with super[BLSwitchAndOrNot].BoolExp;
  class Val(v: boolean)
    extends super[BLShowAndOrNot].Val(v)
       with super[BLSwitchAndOrNot].Val(v)
       with BoolExp;
  class And(l: E, r: E)
    extends super[BLShowAndOrNot].And(l, r)
       with super[BLSwitchAndOrNot].And(l, r)
       with BoolExp;
  class Or(l: E, r: E)
    extends super[BLShowAndOrNot].Or(l, r)
       with super[BLSwitchAndOrNot].Or(l, r)
       with BoolExp;
  class Not(e: E)
    extends super[BLShowAndOrNot].Not(e)
       with super[BLSwitchAndOrNot].Not(e)
       with BoolExp;
}

object Test extends BLShowSwitchAndOrNot with Application {
  type E = BoolExp;
  def Val(v: boolean) = { new Val(v); }
  def And(l: E, r: E) = { new And(l, r); }
  def Or(l: E, r: E) = { new Or(l, r); }
  def Not(e: E) = { new Not(e); }
  val e = And(
            Or(Val(true), Val(false))
          , Not(Val(true))
          );
  val es = e.switchArgs;
  Console.println(es.show + " = " + es.eval);
}


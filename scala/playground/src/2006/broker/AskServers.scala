import concurrent.ops._;

trait AskServers requires (AskServers with Accounts
                                      with MatchServers) {

  def completeAsk(ask: Input): Unit = {
    getAccountFor(ask.client).sell(ask.symbol, ask.amount, ask.price);
  }

  object AskServer {

    private def readInput: Input = {
      Console.print("Enter client name: ");
      val c = Console.readLine;
      Console.print("Enter symbol: ");
      val s = Console.readLine;
      Console.print("Enter amount: ");
      val a = Console.readInt;
      Console.print("Enter price: ");
      val p = Console.readInt;
      new Input {
        val client = c;
        val symbol = s;
        val amount = a;
        val price = p;
      }
    }

    private def worker: Unit = {
      while(true) {
        val in = readInput;
        MatchServer.registerAsk(in);
      }
    }

    def run: Unit = {
      spawn(worker);
    }
  }
}


import concurrent.ops._;

trait BidServers requires (BidServers with Accounts
                                      with MatchServers) {

  def completeBid(bid: Input): Unit = {
    getAccountFor(bid.client).buy(bid.symbol, bid.amount, bid.price);
  }

  object BidServer {

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
        MatchServer.registerBid(in);
      }
    }

    def run: Unit = {
      spawn(worker);
    }
  }
}


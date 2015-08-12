import scala.collection.mutable._;

trait MatchServers requires (MatchServers with AskServers
                                          with BidServers) {
  abstract class Input {
    val client: String;
    val symbol: String;
    val amount: int;
    val price: int;
  }

  object MatchServer {

    val bids = new ArrayBuffer[Input];
    val asks = new ArrayBuffer[Input];

    def registerBid(bid: Input): Unit = {
      val omatch = asks.elements find (x => (x.symbol == bid.symbol) &&
                                            (x.amount == bid.amount) &&
                                            (x.price <= bid.price))
      if (!omatch.isEmpty) {
        val ask = omatch.get;
        completeBid(bid);
        completeAsk(ask);
        // delete match from asks
      } else {
        bids += bid;
      }
    }

    def registerAsk(ask: Input): Unit = {
      val omatch = bids.elements find (x => (x.symbol == ask.symbol) &&
                                            (x.amount == ask.amount) &&
                                            (x.price >= ask.price))
      if (!omatch.isEmpty) {
        val bid = omatch.get;
        completeBid(bid);
        completeAsk(ask);
      } else {
        asks += ask;
      }
    }
  }
}


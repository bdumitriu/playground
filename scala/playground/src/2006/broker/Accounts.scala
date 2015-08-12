trait Accounts {

  class Account(client: String) {
    def buy(symbol: String, amount: int, price: int) = {
      Console.println("Client " + client + " bought " + amount + " " +
        symbol + " shares at " + price + " euro a share.")
    }

    def sell(symbol: String, amount: int, price: int) = {
      Console.println("Client " + client + " sold " + amount + " " +
        symbol + " shares at " + price + " euro a share.")
    }
  }

  def getAccountFor(client: String) = {
    new Account(client);
  }

}

object Test extends Accounts {
  def main(args: Array[String]): Unit = {
    val a1 = new Account("bdumitriu");
    val a2 = new Account("danamrc");
    a1.buy("TLV", 1000, 1);
    a2.sell("TLV", 1000, 1);
  }
}


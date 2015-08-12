object BrokerSystem extends Accounts
                   with MatchServers
                   with BidServers 
                   with AskServers {
  def main(args: Array[String]) = {
    AskServer.run;
    BidServer.run;
  }
}

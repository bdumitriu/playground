//: TIEJ:X1:NonBlockingIO.java
// Socket and selector configuration for non-blocking
// Connects to JabberServer.java
// {RunByHand}
import java.net.*;
import java.nio.channels.*;
import java.util.*;
import java.io.*;
/**
 * Aim: Shows how to use selector. No reading/writing
 *      just shows the readiness of operations.
 *
 * PseudoCode:
 * -> Create a selector.
 * -> Create a channel
 * -> Bind the socket associated with this channel to a
 *    <client-port>
 * -> Configure the channel as non-blocking
 * -> Register the channel with selector.
 * -> Invoke select() so that it blocks until registered
 *    channel is ready. (as opposed to select(long timeout)
 * -> Get the set of keys whose underlying channel is ready
 *    for the operation they showed interest when they
 *    registered with the selector.
 * -> Iterate through the keys.
 * -> For every key check if the underlying channel is ready
 *    for the operation it is interested in.
 * -> If ready print message of readiness.
 *
 * Notes:
 * -> Need MultiJabberServer running on the local machine.
 *    You run it to connect to the local MultiJabberServer
 * -> It causes and exception at MultiJabberServer but
 *    this exception is expected.
 */
public class NonBlockingIO {
  public static void main(String[] args)
    throws IOException {
    if(args.length < 2) {
      System.out.println(
        "Usage: java <client port> <local server port>");
      System.exit(1);
    }
    int cPort = Integer.parseInt(args[0]);
    int sPort = Integer.parseInt(args[1]);
    SocketChannel ch = SocketChannel.open();
    Selector sel = sel = Selector.open();
    try {
      ch.socket().bind(new InetSocketAddress(cPort));
      ch.configureBlocking(false);
      // channel interested in performing read/write/connect
      ch.register(sel, SelectionKey.OP_READ
        | SelectionKey.OP_WRITE | SelectionKey.OP_CONNECT);
      // Unblocks when ready to read/write/connect
      sel.select();
      // Keys whose underlying channel is ready, the
      // operation this channel is interested in can be
      // performed without blocking.
      Iterator it = sel.selectedKeys().iterator();
      while(it.hasNext()) {
        SelectionKey key = (SelectionKey)it.next();
        it.remove();
        // Is underlying channel of key ready to connect?
     // if((key.readyOps() & SelectionKey.OP_CONNECT) != 0) {
        if(key.isConnectable()) {
          InetAddress ad = InetAddress.getLocalHost();
          System.out.println("Connect will not block");
          //You must check the return value of connect to make
          //sure that it has connected. This call being
          //non-blocking may return without connecting when
          //there is no server where you are trying to connect
          //Hence you call finishConnect() that finishes the
          //connect operation.
          if(!ch.connect(new InetSocketAddress(ad, sPort)))
            ch.finishConnect();
        }
        // Is underlying channel of key ready to read?
        // if((key.readyOps() & SelectionKey.OP_READ) != 0)
        if(key.isReadable())
          System.out.println("Read will not block");
        // Is underlying channel of key ready to write?
        // if((key.readyOps() & SelectionKey.OP_WRITE) != 0)
        if(key.isWritable())
          System.out.println("Write will not block");
      }
    } finally {
      ch.close();
      sel.close();
    }
  }
} ///:~

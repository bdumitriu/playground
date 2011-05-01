//: TIEJ:X1:MultiJabberServer1.java
// Has the same semantics as multi-threaded
// MultiJabberServer
// {RunByHand}
import java.io.*;
import java.net.*;
import java.nio.*;
import java.nio.channels.*;
import java.nio.charset.*;
import java.util.*;
/**
 * The Server accepts connections in non-blocking fashion.
 * A connection when established, a socket is created,
 * which is registered for read/write with the selector.
 * Reading/Writing is performed on this socket when the
 * selector unblocks.
 * This program works exactly the same way as MultiJabberServer.
 */
public class MultiJabberServer1 {
  public static final int PORT = 8080;
  public static void main(String[] args)
      throws IOException {
    //Channel read from data will be in ByteBuffer form
    //written by PrintWriter.println(). Decoding of this
    //byte stream requires character set of default encoding.
    String encoding = System.getProperty("file.encoding");
    //Had to initialized here since we do not wish to create
    //a new instance of Charset everytime it is required
    //Charset cs = Charset.forName(
    //  System.getProperty("file.encoding"));
    Charset cs = Charset.forName(encoding);
    ByteBuffer buffer = ByteBuffer.allocate(16);
    SocketChannel ch = null;
    ServerSocketChannel ssc = ServerSocketChannel.open();
    Selector sel = Selector.open();
    try {
      ssc.configureBlocking(false);
      //Local address on which it will listen for connections
      //Note: Socket.getChannel() returns null unless a channel
      //is associated with it as shown below.
      //i.e the expression (ssc.socket().getChannel() != null) is true
      ssc.socket().bind(new InetSocketAddress(PORT));
      // Channel is interested in OP_ACCEPT events
      SelectionKey key =
        ssc.register(sel, SelectionKey.OP_ACCEPT);
      System.out.println("Server on port: " + PORT);
      while(true) {
        sel.select();
        Iterator it = sel.selectedKeys().iterator();
        while(it.hasNext()) {
          SelectionKey skey = (SelectionKey)it.next();
          it.remove();
          if(skey.isAcceptable()) {
            ch = ssc.accept();
            System.out.println(
              "Accepted connection from:" + ch.socket());
            ch.configureBlocking(false);
            ch.register(sel, SelectionKey.OP_READ);
          } else {
            // Note no check performed if the channel
            // is writable or readable - to keep it simple
            ch = (SocketChannel)skey.channel();
            ch.read(buffer);
            CharBuffer cb = cs.decode(
              (ByteBuffer)buffer.flip());
            String response = cb.toString();
            System.out.print("Echoing : " + response);
            ch.write((ByteBuffer)buffer.rewind());
            if(response.indexOf("END") != -1) ch.close();
            buffer.clear();
          }
        }
      }
    } finally {
      if(ch != null) ch.close();
      ssc.close();
      sel.close();
    }
  }
} ///:~
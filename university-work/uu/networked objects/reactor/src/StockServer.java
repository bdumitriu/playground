import java.io.IOException;
import java.net.InetSocketAddress;
import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;
import java.nio.channels.ServerSocketChannel;
import java.nio.channels.SocketChannel;
import java.nio.charset.CharacterCodingException;
import java.nio.charset.Charset;
import java.nio.charset.CharsetDecoder;
import java.util.HashMap;
import java.util.Iterator;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class StockServer
{
    public static final int PORT = 10000;

    private HashMap<String, Stock> stocks;

    public static void main(String args[]) throws IOException
    {
        new StockServer();
    }

    public StockServer() throws IOException
    {
        initializeStocks();

        BlockingQueue<SocketChannel> channelQueue;
        channelQueue = new LinkedBlockingQueue<SocketChannel>();

        Selector commSelector = Selector.open();

        new AcceptorThread(PORT, commSelector, channelQueue).start();
        new CommunicationThread(commSelector, channelQueue, stocks).start();
    }
    
    private void initializeStocks()
    {
        stocks = new HashMap<String, Stock>();

        stocks.put("CSCO", new Stock("CSCO", "CISCO SYS INC", 17.47, 17.47, 82869070));
        stocks.put("MSFT", new Stock("MSFT", "MICROSOFT CP", 27.28, 27.28, 51945506));
        stocks.put("DELL", new Stock("DELL", "DELL INC", 29.40, 29.40, 50636645));
        stocks.put("INTC", new Stock("INTC", "INTEL CP", 25.13, 25.13, 40317625));
        stocks.put("ORCL", new Stock("ORCL", "ORACLE CORP", 12.81, 12.81, 30479264));
        stocks.put("JDSU", new Stock("JDSU", "JDS UNIPHASE CP", 2.29, 2.29, 21820431));
        stocks.put("PTEN", new Stock("PTEN", "PATTERSON-UTI ENER", 30.00, 30.00, 21773211));
        stocks.put("SUNW", new Stock("SUNW", "SUN MICROSYS INC", 3.70, 3.70, 20790943));
        stocks.put("AMAT", new Stock("AMAT", "APPLIED MATERIALS", 17.96, 17.96, 17243503));
        stocks.put("CPST", new Stock("CPST", "CAPSTONE TURBINE C", 3.13, 3.13, 16493719));
        stocks.put("AAPL", new Stock("AAPL", "APPLE COMPUTER", 61.54, 61.54, 15196067));
        stocks.put("SIRI", new Stock("SIRI", "SIRIUS SATELLITE R", 7.00, 7.00, 14892773));
        stocks.put("EBAY", new Stock("EBAY", "EBAY INC", 43.89, 43.89, 14200480));
        stocks.put("QCOM", new Stock("QCOM", "QUALCOMM INC", 45.42, 45.42, 12494535));
        stocks.put("YHOO", new Stock("YHOO", "YAHOO INC", 38.49, 38.49, 12234337));
        stocks.put("PLAY", new Stock("PLAY", "PORTALPLAYER INC", 24.17, 24.17, 10873040));
        stocks.put("JNPR", new Stock("JNPR", "JUNIPER NETWORKS", 23.99, 23.99, 10363597));
        stocks.put("CMCSK", new Stock("CMCSK", "COMCAST CL A SPCL", 26.29, 26.29, 9818082));
        stocks.put("SYMC", new Stock("SYMC", "SYMANTEC CP", 19.61, 19.61, 9732226));
        stocks.put("CMCSA", new Stock("CMCSA", "COMCAST CP A", 26.93, 26.93, 9635642));
        stocks.put("SNDK", new Stock("SNDK", "SANDISK CP", 60.98, 60.98, 9088704));
        stocks.put("ATML", new Stock("ATML", "ATMEL CORP", 2.66, 2.66, 8068010));
        stocks.put("SRNA", new Stock("SRNA", "SERENA SOFTWARE IN", 23.50, 23.50, 7765246));
        stocks.put("RIMM", new Stock("RIMM", "RESEARCH IN MOTION", 66.79, 66.79, 7660668));
    }
}

class AcceptorThread extends Thread
{
    private Selector acceptSelector;
    private Selector commSelector;
    private BlockingQueue<SocketChannel> channelQueue;

    public AcceptorThread(int port, Selector commSelector,
            BlockingQueue<SocketChannel> channelQueue)
        throws IOException
    {
        super();

        this.commSelector = commSelector;
        this.channelQueue = channelQueue;

        ServerSocketChannel ssc = ServerSocketChannel.open();
        ssc.configureBlocking(false);

        InetSocketAddress address = new InetSocketAddress(port);
        ssc.socket().bind(address);

        System.out.println("Bound to " + address);

        acceptSelector = Selector.open();
        ssc.register(acceptSelector, SelectionKey.OP_ACCEPT);
    }

    public void run()
    {
        while (true)
        {
            try
            {
                System.out.println("Acceptor thread: Selecting");

                acceptSelector.select();

                process();
            }
            catch (Exception ex)
            {
                ex.printStackTrace();
            }
        }
    }

    private void process() throws Exception
    {
        Iterator<SelectionKey> keysIterator;
        keysIterator = acceptSelector.selectedKeys().iterator();

        while (keysIterator.hasNext())
        {
            SelectionKey key = keysIterator.next();
            keysIterator.remove();

            ServerSocketChannel acceptChannel = (ServerSocketChannel) key.channel();
            SocketChannel commChannel = acceptChannel.accept();

            System.out.println("Acceptor thread: Connection from "
                    + commChannel.socket().getInetAddress());

            channelQueue.put(commChannel);
            commSelector.wakeup();
        }
    }
}

class CommunicationThread extends Thread
{
    private static final int READ_BUFFER_SIZE = 16;
    private Selector commSelector;
    private BlockingQueue<SocketChannel> channelQueue;
    private HashMap<String, Stock> stocks;
    private ByteBuffer readBuffer;
    private Charset charset;
    private CharsetDecoder decoder;

    public CommunicationThread(Selector commSelector,
            BlockingQueue<SocketChannel> channelQueue, HashMap<String, Stock> stocks)
        throws IOException
    {
        super();

        this.commSelector = commSelector;
        this.channelQueue = channelQueue;
        this.stocks = stocks;

        this.readBuffer = ByteBuffer.allocateDirect(READ_BUFFER_SIZE);

        String encoding = System.getProperty("file.encoding");
        charset = Charset.forName(encoding);
        decoder = charset.newDecoder();
    }

    public void run()
    {
        while (true)
        {
            try
            {
                System.out.println("Communication thread: Selecting");

                registerNewChannels();

                commSelector.select();

                process();
            }
            catch (Exception ex)
            {
                ex.printStackTrace();
            }
        }
    }

    private void registerNewChannels() throws Exception
    {
        SocketChannel channel;
        while ((channel = channelQueue.poll()) != null)
        {
            channel.configureBlocking(false);
            channel.register(commSelector, SelectionKey.OP_READ, new StringBuilder());
        }
    }

    private void process() throws Exception
    {
        Iterator<SelectionKey> keysIterator;
        keysIterator = commSelector.selectedKeys().iterator();

        while (keysIterator.hasNext())
        {
            SelectionKey key = (SelectionKey) keysIterator.next();
            keysIterator.remove();

            if (key.isReadable())
            {
                processRequest(key);
            }
            if (key.isWritable())
            {
                processCompletedRequest(key);
            }
        }
    }

    private void processRequest(SelectionKey key) throws Exception
    {
        SocketChannel commChannel = (SocketChannel) key.channel();

        try
        {
            boolean eof = commChannel.read(readBuffer) == -1;

            readBuffer.flip();
            String result = decoder.decode(readBuffer).toString();
            readBuffer.clear();

            System.out.println("Communication thread: Processing partial request");
            System.out.println("---");
            System.out.println(result);
            System.out.println("---");

            StringBuilder requestString = (StringBuilder) key.attachment();
            requestString.append(result);

            String request = requestString.toString();

            if (request.endsWith("\n\n") || request.endsWith("\r\n\r\n") || eof)
            {
                int oldInterestOps = key.interestOps(); 
                int newInterestOps = oldInterestOps | SelectionKey.OP_WRITE;
                key.interestOps(newInterestOps);
            }
        }
        catch (IOException ioe)
        {
            sendError(commChannel, "An internal server error occured while processing your request.");
        }
    }

    private void processCompletedRequest(SelectionKey key)
        throws IOException
    {
        String request = key.attachment().toString();
        SocketChannel commChannel = (SocketChannel) key.channel();

        System.out.println("Communication thread: Processing full request");
        System.out.println("---");
        System.out.println(request);
        System.out.println("---");

        // looking for something like: <get-quote><symbol>CSCO</symbol></get-quote>
        // with random whitespace in between
        Pattern p =
            Pattern.compile("\\s*<get-quote>\\s*<symbol>\\s*([A-Z]+)\\s*</symbol>\\s*</get-quote>\\s*");
        Matcher m = p.matcher(request);

        if (m.matches())
        {
            Stock s = stocks.get(m.group(1));
            if (s != null)
            {
                ByteBuffer response = s.toByteBuffer(charset);
                commChannel.write(response);
            }
            else
            {
                sendError(commChannel, "Stock symbol " + m.group(1) + " not found.");
            }
        }
        else
        {
            sendError(commChannel, "Badly formed request.");
        }

        commChannel.close();
    }

    private void sendError(SocketChannel channel, String message)
        throws IOException
    {
        StringBuilder temp = new StringBuilder();
        temp.append("<error>");
        temp.append(message);
        temp.append("</error>\n\n");

        CharBuffer result = CharBuffer.allocate(temp.length());
        result.put(temp.toString());
        result.flip();

        ByteBuffer buffer = charset.newEncoder().encode(result);

        channel.write(buffer);
    }
}

class Stock
{
    private String symbol;
    private String companyName;
    private double lastPrice;
    private double openingPrice;
    private int volumeTraded;

    public Stock(String symbol, String companyName, double lastPrice, double openingPrice, int volumeTraded)
    {
        this.symbol = symbol;
        this.companyName = companyName;
        this.lastPrice = lastPrice;
        this.openingPrice = openingPrice;
        this.volumeTraded = volumeTraded;
    }

    public ByteBuffer toByteBuffer(Charset charset) throws CharacterCodingException
    {
        StringBuilder temp = new StringBuilder();
        temp.append("<quote>\n");
        temp.append("\t<symbol>");
        temp.append(symbol);
        temp.append("</symbol>\n");
        temp.append("\t<company-name>");
        temp.append(companyName);
        temp.append("</company-name>\n");
        temp.append("\t<last-price>");
        temp.append(lastPrice);
        temp.append("</last-price>\n");
        temp.append("\t<opening-price>");
        temp.append(openingPrice);
        temp.append("</opening-price>\n");
        temp.append("\t<volume>");
        temp.append(volumeTraded);
        temp.append("</volume>\n");
        temp.append("</quote>\n\n");

        CharBuffer result = CharBuffer.allocate(temp.length());
        result.put(temp.toString());
        result.flip();

        ByteBuffer buffer = charset.newEncoder().encode(result);

        return buffer;
    }

    public double getLastPrice()
    {
        return lastPrice;
    }

    public void setLastPrice(double lastPrice)
    {
        this.lastPrice = lastPrice;
    }

    public String getCompanyName()
    {
        return companyName;
    }

    public void setCompanyName(String companyName)
    {
        this.companyName = companyName;
    }

    public double getOpeningPrice()
    {
        return openingPrice;
    }

    public void setOpeningPrice(double openingPrice)
    {
        this.openingPrice = openingPrice;
    }

    public String getSymbol()
    {
        return symbol;
    }

    public void setSymbol(String symbol)
    {
        this.symbol = symbol;
    }

    public int getVolumeTraded()
    {
        return volumeTraded;
    }

    public void setVolumeTraded(int volumeTraded)
    {
        this.volumeTraded = volumeTraded;
    }
}

package gw.blog.test;


import gw.GwContext;
import gw.render.ParserFactory;
import gw.render.StylesheetFactory;
import gw.render.parsers.Parser;

import java.util.ResourceBundle;

import com.mockobjects.util.AssertMo;

public class MockGwContext extends GwContext {
    
    ParserFactory parserFactory;

    public MockGwContext() {
        super(null);
        parserFactory = new ParserFactory();
    }
    
    public synchronized ParserFactory getParserFactory() {
        return parserFactory;     
    }
    
    public void registerParser(String inputContentType, String outputContentType, Parser parser) {
        parserFactory.register(inputContentType, outputContentType, parser);
    }


    public synchronized StylesheetFactory getStylesheetFactory() {
        notImplemented();
        return null;
    }
    
    public synchronized ResourceBundle getTextResources() {
        notImplemented();
        return null;
    }

    public synchronized ResourceBundle getConfigResources() {
        notImplemented();
        return null;
    }
       
    public String getApplicationPrefix() {
        notImplemented();
        return null;
    }

    public void log(String msg) {
        notImplemented();    
    }
    
    public void log(String msg, Throwable throwable) {
        notImplemented();    
    }
    
    
    public static void notImplemented() {
        AssertMo.notImplemented(MockGwContext.class.getName());
    }

}

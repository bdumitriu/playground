package gw.blog.test;

import java.io.InputStream;
import java.io.Reader;

import org.jdom.Element;

import gw.render.parsers.ParseException;
import gw.render.parsers.Parser;

import com.mockobjects.MockObject;

public class MockParser extends MockObject implements Parser {

    public Object parse(Reader reader) throws ParseException {
        notImplemented();
        return null;
    }

    public Object parse(InputStream inputStream) throws ParseException {
        notImplemented();
        return null;
    }

    public Object parse(String string) throws ParseException {
        Element result = new Element("test");
        result.setText(string);
        return result;
    }

}

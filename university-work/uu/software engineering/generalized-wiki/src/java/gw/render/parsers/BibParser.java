package gw.render.parsers;

import gw.render.parsers.ParseException;
import gw.render.parsers.Parser;
import java.util.regex.*;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.StringReader;
import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.File;

import org.jdom.Element;
import org.jdom.input.SAXBuilder;

public class BibParser implements Parser {

    private static BibParser bibParser;
    
    /**
     * @return The instance of this parser 
     */
    public static BibParser getInstance(){
    	if (bibParser==null) bibParser = new BibParser();
    	return bibParser;
    }

	/**
     * Parse the given XML content
     * 
     * @param reader
     *            The Reader that contains the input to parse
     * @return Element The parsed input
     * @throws ParseException
     */
    public Object parse(Reader reader) throws ParseException {
        Element result = null;
        try {
            String inputFile  = generateFileName()+".bib";  
            String atermFile  = generateFileName()+".abib";
            //String outputFile = generateFileName()+".xml";
                                                       
            
            // Save the textual input submitted by the user in a text file
            BufferedWriter bw = new BufferedWriter(new FileWriter(inputFile));
            BufferedReader br = new BufferedReader(reader);
            String buff = null;
                        
            StringBuffer bibBuffer = new StringBuffer();
            String regexNames = "(?:editor|author\\s*\\=\\s*(\\{|\"))(\\{\\\\sortunder\\{.*\\}\\})?(((\\s*\\w+\\.?\\s*)*,?)*)";
            String regexDelim = "(year\\s*\\=\\s*)(\\d+)";
            try {               
                while((buff = br.readLine())!=null)
                {
                    bibBuffer.append(buff);                 
                }                              
                String bibText = bibBuffer.toString();             
                Pattern p  = Pattern.compile(regexNames);             
                Matcher m  = p.matcher(bibText);
                while(m.find())
                {
                    
                    if(m.group(3).length()>1)
                        bibText = bibText.replace(m.group(3),reformatNames(m.group(3)));
                }
                m.reset();
                p  = Pattern.compile(regexDelim);
                m = p.matcher(bibText);
                while(m.find())
                {
                    
                    bibText = bibText.replace(m.group(),m.group().replaceAll("\\d+","{$0}"));
                }                           
                bw.write(bibText);
                bw.flush();
            }
            catch(Exception E) {}
            br.close();
            bw.close();                        
            
            // Todo: link processes in pipe together.
            Process proc;
            proc = Runtime.getRuntime().exec("bib-transform -i " + inputFile + " -o "+ atermFile +" --normalize");
            proc.waitFor();
            proc = Runtime.getRuntime().exec("aterm2xml -i " + atermFile );
            
            
            // Lezen van het proces
            //InputStream in = proc.getInputStream();
            br = new BufferedReader(new InputStreamReader(proc.getInputStream()));
            StringBuffer xmlBuffer = new StringBuffer();            
            try {                
                while((buff = br.readLine())!=null)
                {                   
                    xmlBuffer.append(buff);
                }                    
            }
            catch (IOException e) {
                System.exit(0);
            }            
            br.close();
            String tempXML = xmlBuffer.toString();
            System.out.println(tempXML);
            //reset the buffer
            xmlBuffer.delete(0,xmlBuffer.length());
                                                       
            String regexp = "<Field><at:string>abstract</at:string><Words><at:list>.*?</at:list></Words></Field>"; 
            Pattern p  = Pattern.compile(regexp);
            Matcher m  = p.matcher(tempXML);            
            while(m.find())
            {
                m.appendReplacement(xmlBuffer, "");
            }
            m.appendTail(xmlBuffer);
            tempXML = xmlBuffer.toString();
//          Replace all the occurances of "at:" in the XML by ""
            regexp = "<at:"; //" Specifying the format: XML explained" 
            tempXML = tempXML.replaceAll(regexp, "<");
            regexp = "</at:";  
            tempXML = tempXML.replaceAll(regexp, "</");
            regexp = "<\\?xml version=\"1.0\" \\?>";  
            tempXML = tempXML.replaceAll(regexp, "");            
         
            //System.out.println(tempXML);
            // Get the root element of the output
            result = new SAXBuilder().build(new StringReader(tempXML)).getRootElement();
                                    
            // remove temporal files
            removeTempFile(inputFile);
            removeTempFile(atermFile);                                    
        } catch (Exception e) {
            throw new ParseException(e);
        }
        return result;
    }
    
    /**
     * generate a 'unique' filename from system current time and ramdom integers.
     * @return String
     */
    public String generateFileName(){
        double random = Math.random();
        int randomPrefix = (int) (random * 31);
        int randomSuffix = (int) (random * 521);
        int randomChar   = (int) (System.currentTimeMillis() / ((Math.random()+1)*2083)); 
        return randomPrefix + randomChar + randomSuffix+"";
    }
    
    /**
     * remove temporal files generated before constructing the final xml file.
     * @param name
     */
    private void removeTempFile(String name){
        File tmp = new File(name);
        if(tmp.exists())
            tmp.delete();       
    }
    
    /**
     * rewrite names as `firstname von lastname` separated by `and`.
     * @param str
     * @return Stringbuffer
     */
    public StringBuffer reformatNames(String str)
    {
        String[] parts = str.split("\\band\\b");
        StringBuffer buf = new StringBuffer();
        int size = parts.length;
        int index = 0;   
        for (int i=0; i< size ; i++)
        {
            index = parts[i].indexOf(",");
            if(index > -1 )
            {

                buf.append(parts[i].substring(index+1).trim());
                buf.append(" ");
                buf.append(parts[i].substring(0,index).trim());
                parts[i] = buf.toString();
                buf.delete(0,buf.length());
            }
        }
        for (int i=0; i<size; i++) {
            buf.append(parts[i] + (i == size -1 ? " " : " and "));
        }
        return buf;
    }

    /**
     * Parse the given XML content
     * 
     * @param inputStream
     *            The InputStream that contains the input to parse
     * @return Element The parsed input
     * @throws ParseException
     */
    public Object parse(InputStream inputStream) throws ParseException {
        return parse(new InputStreamReader(inputStream));
    }

    /**
     * Parse the given XML content
     * 
     * @param string
     *            The String that contains the input to parse
     * @return Element The parsed input
     * @throws ParseException
     */
    public Object parse(String string) throws ParseException {
        return parse(new StringReader(string));
    }

}


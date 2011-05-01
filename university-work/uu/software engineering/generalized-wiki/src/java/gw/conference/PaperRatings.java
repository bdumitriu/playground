/*
 * Created on Oct 24, 2005
 *
 * TODO To change the template for this generated file go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
package gw.conference;

import gw.render.parsers.ParseException;
import gw.render.parsers.XMLParser;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStreamWriter;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Map;

import org.jdom.Attribute;
import org.jdom.Element;
import org.jdom.JDOMException;
import org.jdom.output.XMLOutputter;
import org.jdom.xpath.XPath;

 
public class PaperRatings {
    
    private Map<String, RatingSet> _ratingsTotal;
    private Element _xmlTree;
    
    // For optimization
    private XMLOutputter _xmlOutputter;
    
    // Builds a new paperRating for a paper
    public PaperRatings() {
        Element root = new Element("conference");       
        this._xmlTree = root;
        
    }
    
    // Builds paperRatings from an existing xml file.
    public void construct(InputStream stream) throws ParseException {
        this._xmlTree = (org.jdom.Element) XMLParser.getInstance().parse(stream);
    }
    
    public void construct(String s) throws ParseException {       
        byte[] tmp = s.getBytes();
        InputStream is = new ByteArrayInputStream(tmp);   
        this._xmlTree = (org.jdom.Element) XMLParser.getInstance().parse(is);
    }
    
    /******************************************************************************************************
     * Handles the inserting of new papers in the conference to rate                                      *
     ******************************************************************************************************/
    
    public void addPaper(String name) {
        Element newPaper = new Element("paper");
        newPaper.setAttribute("name", name);
        
        this._xmlTree.addContent(newPaper);
    }

    /******************************************************************************************************
    * Handles adding and updating of ratings                                                              *
    ******************************************************************************************************/
    public void update(String paper, String reviewer, String date, RatingSet ratings) throws JDOMException {

        Element rater = findRaterElement(paper, reviewer);
        
        // Remove the nodes
        rater.removeContent();
        
        // Add the new content
        rater.addContent(constructRatingElements(ratings));

    }

    public void add(String paper, String reviewer, String date, RatingSet ratings) throws JDOMException {
        findPaperElement(paper).addContent(constructRaterElement(reviewer, date));
        
        update(paper, reviewer, date, ratings);
    }
    
    private Element constructRaterElement(String reviewer, String date) {
        Element raterElement = new Element("rater");
        raterElement.setAttribute("user", reviewer);
        raterElement.setAttribute("date", date);
        return raterElement;
    }
    
    private Element findPaperElement(String paper) throws JDOMException {
        String xPathExpression = "/conference-rating/paper[@name='" + paper + "']";
        return (Element) XPath.selectSingleNode(this._xmlTree, xPathExpression);
    }
    private Element findRaterElement(String paper, String reviewer) throws JDOMException {
        String xPathExpression = "/rater[@user='" + reviewer + "']";   
        return (Element) XPath.selectSingleNode(findPaperElement(paper), xPathExpression);
    }
    
    private Collection constructRatingElements(RatingSet ratings) {
        ArrayList<Element> ratingElements = new ArrayList<Element>();
        for(Rating rating : ratings) {
            ratingElements.add(constructRatingElement(rating));
        }
        return ratingElements; 
    }
    
    private Element constructRatingElement(Rating rating) {
        return  new Element("rating").setAttributes(constructAttributeList(rating));
    }
    
    private java.util.List constructAttributeList(Rating rating) {
        ArrayList<Attribute> attributeList = new ArrayList<Attribute>();
        attributeList.add(new Attribute("name", rating.getName()));
        attributeList.add(new Attribute("value", rating.getValue()));
        return attributeList;
    }
    
    
     /******************************************************************************************************
     * Exports the xml-tree as text                                                                        *
     * @throws IOException 
     ******************************************************************************************************/
    public void writeToStream(OutputStreamWriter output) throws IOException {
        
        if( this._xmlOutputter == null) {
            this._xmlOutputter= new XMLOutputter();
        }
        this._xmlOutputter.output(this._xmlTree, output );
    }
    
    public String toString() {
        String toReturn = "";
        
        for( String key : this._ratingsTotal.keySet()) {
            toReturn += "\n" + key + "\n";
            toReturn += this._ratingsTotal.get(key).toString();
        }
        
        return toReturn;
    }

}

package gw.render;

import javax.xml.transform.Source;

public class TransformationResource {
	
	private Stylesheet stylesheet;
	private Source source;
	
	
	
	public TransformationResource(Stylesheet stylesheet, Source source) {
		this.stylesheet = stylesheet;
		this.source = source;
	}
	
	public Source getSource() {
		return source;
	}
	
	public void setSource(Source source) {
		this.source = source;
	}
	
	public Stylesheet getStylesheet() {
		return stylesheet;
	}
	
	public void setStylesheet(Stylesheet stylesheet) {
		this.stylesheet = stylesheet;
	}	
}

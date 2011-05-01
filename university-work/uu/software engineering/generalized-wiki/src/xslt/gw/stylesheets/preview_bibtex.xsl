<?xml version="1.0"?>
<!DOCTYPE stylesheet
 [
  <!ENTITY nbsp "&#160;">
 ]
>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                              xmlns:xalan="http://xml.apache.org/xalan"
			      xmlns:bibtex="gw.util.BibTex"
			      xmlns:bib="gw.render.extensions.ExtensionFunctions"
                              extension-element-prefixes="bibtex bib xalan">
  

  <xsl:output method="html" indent="yes"/>
  <xsl:strip-space elements="*"/>
  <xsl:include href="resource:/gw/stylesheets/lib/bibtex_common.xsl"/>
  <xsl:include href="resource:/gw/stylesheets/lib/preview-form.xsl" />
  <xsl:variable name="sortField1" select="" />
  <xsl:param name="pathinfo"/>
  

  <xsl:template match="/Entries">
  <html>
    <head>
      <title>Preview of <xsl:value-of select="$pathinfo"/></title>
      <xsl:call-template name="css_header"/>
    </head>
    <body>
	
	<div class="gwLeftBar">This is a leftbar</div>
	<div class="gwTopic">

		<xsl:call-template name="viewEntries"/>

        	<xsl:apply-imports/>
        	
		<pre width="125"><xsl:value-of select="bib:include($pathinfo, 'bibtex:text/plain')"/></pre>

        	<xsl:call-template name="previewForm"/>
	</div>
	  
   </body>
 </html>
 </xsl:template>
 
</xsl:stylesheet>

<?xml version="1.0"?>

<xsl:stylesheet version="1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:gw="http://www.cs.uu.nl/wiki/Gw"
  xmlns:xhtml="http://www.w3.org/1999/xhtml"
  xmlns="http://www.w3.org/1999/xhtml"
  exclude-result-prefixes="gw xhtml">
  
  <xsl:import href="resource:/gw/stylesheets/lib/master.xsl" />
  <xsl:param name="pathinfo"/>
  
  <xsl:output method="xml" media-type="application/xhtml+xml"/>
  
  <xsl:template match="editForm">
    <html>
      <head>
	<title>Edit <xsl:value-of select="$pathinfo" /></title>
    <xsl:call-template name="css_header"/>
      </head>
      <body>
      <div class="gwTopic gwEditPage">
      <h1>Edit <xsl:value-of select="$pathinfo" /></h1>
    
    <form name="edit" action="{actionPath}" method="post">
      <p>
	<textarea id="taBody" name="text" wrap="virtual" rows="35" cols="100" style="width: 99%; font-family : monospace;">
	  <xsl:value-of select="content" />
	</textarea>
	ContentType:
	<input id="contenttype" name="contenttype" type="text" value="{contentTypeValue}"/>
    </p>
    <p>
    Validate: 
    <input id="validate" type="checkbox" name="validate" value="true"/>  
    </p>

      <input id="btnPreview" type="submit" value="Preview" />
      <a href="/gw/view{$pathinfo}">Cancel this edit</a>
    </form>
	  </div>
      </body>
    </html>
  </xsl:template>
  
</xsl:stylesheet>

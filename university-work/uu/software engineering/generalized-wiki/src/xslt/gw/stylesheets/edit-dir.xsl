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
  
  <xsl:template match="/">
    <html>
      <head>
	<title>Edit <xsl:value-of select="$pathinfo" /></title>
	<xsl:call-template name="css_header"/>
      </head>
      <body>
      <div class="gwTopic">
		 Directories can not be editted.  <a href="/gw/view{$pathinfo}">Go back</a>.
	  </div>
      </body>
    </html>
  </xsl:template>

</xsl:stylesheet>

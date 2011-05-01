<?xml version="1.0"?>

<xsl:stylesheet version="1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:gw="http://www.cs.uu.nl/wiki/Gw"
  xmlns:xhtml="http://www.w3.org/1999/xhtml"
  xmlns="http://www.w3.org/1999/xhtml"
  exclude-result-prefixes="gw xhtml">

  <xsl:import href="resource:/gw/stylesheets/lib/master.xsl" />
  <xsl:variable name="pathinfo" select="/view/pathinfo"/>
  <xsl:output method="xml" media-type="application/xhtml+xml"/>

  <xsl:template match="/">
    <html>
      <head>
	<title>Preview <xsl:value-of select="{$pathinfo}" /></title>
	<meta http-equiv="Pragma" content="no-cache" />
	<meta http-equiv="Expires" content="-1" />
	<xsl:call-template name="css_header"/>
      </head>
      <body>
      <div class="gwTopic">
	Directories can not be previewed. <a href="/gw/view{$pathinfo}">Go back</a>.
	  </div>
      </body>
    </html>
  </xsl:template>

</xsl:stylesheet>

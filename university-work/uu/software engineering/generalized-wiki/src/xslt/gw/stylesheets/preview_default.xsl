<?xml version="1.0"?>

<!-- Changed to new GWML -->

<xsl:stylesheet version="1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:gw="http://www.cs.uu.nl/wiki/Gw"
  xmlns:xhtml="http://www.w3.org/1999/xhtml"
  xmlns="http://www.w3.org/1999/xhtml"
  exclude-result-prefixes="gw xhtml">
  
  	<xsl:import href="resource:/gw/stylesheets/lib/master.xsl" />
  	<xsl:include href="resource:/gw/stylesheets/lib/preview-form.xsl" />
  
  	<xsl:output method="html" indent="yes"/>
  	<xsl:param name="pathinfo"/>

	<xsl:template match="document">
		<html>
		<head>
			<title>Preview of <xsl:value-of select="$pathinfo"/></title>
			<xsl:call-template name="css_header"/>
			<meta http-equiv="Pragma" content="no-cache" />
			<meta http-equiv="Expires" content="-1" />
		</head>
		
		<body>
		<div class="document">
			<h1>This is a PREVIEW of <xsl:value-of select="$pathinfo"/></h1>
			<xsl:apply-templates select="head" />
			<xsl:apply-templates select="section | toc" />
			<xsl:call-template name="previewForm"/>
		</div>
		
		</body>
		</html>
	</xsl:template>  
  
</xsl:stylesheet>

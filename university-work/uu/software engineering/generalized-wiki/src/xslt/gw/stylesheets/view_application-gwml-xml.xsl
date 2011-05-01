<?xml version="1.0"?>

<!-- Changed to new GWML -->

<xsl:stylesheet version="1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:gw="http://www.cs.uu.nl/wiki/Gw"
  xmlns:xhtml="http://www.w3.org/1999/xhtml"
  xmlns="http://www.w3.org/1999/xhtml"
  exclude-result-prefixes="gw xhtml"
  
  xmlns:xalan="http://xml.apache.org/xalan"
  xmlns:repep="gw.render.extensions.ExtensionFunctions"
  extension-element-prefixes="repep xalan"
  >
  
  	<xsl:import href="resource:/gw/stylesheets/lib/master.xsl" />
  	<xsl:include href="resource:/gw/stylesheets/lib/statusbar.xsl" />
  
  	<xsl:output method="html" indent="yes"/>
  	<xsl:param name="pathinfo"/>

	<xsl:template match="document">
		<html>
		<head>
			<xsl:call-template name="css_header"/>
		</head>
		
		<body>
		<div class="gwLeftBar">This is a leftbar</div>
		<div class="gwTopic">
			
			<xsl:apply-templates select="head" />
			<xsl:apply-templates select="section | toc" />
			<!--xsl:value-of select="repep:include($pathinfo, 'text:html')"/-->
			
			<xsl:call-template name="statusbar"/>
		</div>
		
		</body>
		</html>
	</xsl:template>  
	
</xsl:stylesheet>


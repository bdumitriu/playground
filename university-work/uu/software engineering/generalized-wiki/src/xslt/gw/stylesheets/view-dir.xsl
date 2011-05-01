<?xml version="1.0"?>

<xsl:stylesheet version="1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:gw="http://www.cs.uu.nl/wiki/Gw"
  xmlns:xhtml="http://www.w3.org/1999/xhtml"
  xmlns="http://www.w3.org/1999/xhtml"
  exclude-result-prefixes="gw xhtml"
  xmlns:xalan="http://xml.apache.org/xalan"
  xmlns:extFunct="gw.render.extensions.ExtensionFunctions"
  extension-element-prefixes="extFunct xalan">

  <xsl:import href="resource:/gw/stylesheets/lib/master.xsl" />
  <xsl:include href="resource:/gw/stylesheets/lib/statusbar.xsl" />
  
  <xsl:output method="html" indent="yes"/>
  <xsl:param name="pathinfo"/>

  	<xsl:template match="document">
    	<html>
      		<head>
				<title>Directory Listing</title>
    			<xsl:call-template name="css_header"/>
      		</head>
      		<body>
      			<div class="gwTopic">      
					<h1 align="left">Listing of <xsl:value-of select="section/title" /></h1>
					<table border="0" frame="hsides" rules="cols">
	  					<xsl:apply-templates select="section" />
					</table>
					<xsl:call-template name="statusbar"/>
				</div>
      		</body>
    	</html>
  	</xsl:template>
  	
  	<xsl:template match="section">
	  	<xsl:for-each select="link">
		    <tr>
		      <td> 
				<a href="{extFunct:getUrl('view', concat('/', @target))}"><xsl:value-of select="@target" /></a> 
		      </td>
		      <td>
			<small><a href="{extFunct:getUrl('status', concat('/', @target))}">status</a></small> - 
			<small><a href="{extFunct:getUrl('edit', concat('/', @target))}">edit</a></small>
		      </td>
		    </tr>
	  	</xsl:for-each>
  	</xsl:template>  
</xsl:stylesheet>
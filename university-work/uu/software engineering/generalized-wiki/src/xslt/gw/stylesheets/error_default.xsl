<?xml version="1.0"?>

<xsl:stylesheet version="1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:gw="http://www.cs.uu.nl/wiki/Gw"
  xmlns:xhtml="http://www.w3.org/1999/xhtml"
  xmlns="http://www.w3.org/1999/xhtml"
  exclude-result-prefixes="gw xhtml">

  <xsl:param name="pathinfo"/>
  
  <xsl:import href="resource:/gw/stylesheets/lib/master.xsl" />
  <xsl:output method="html" media-type="application/xhtml+xml"/>

   <xsl:template match ="error">
        <html xmlns="http://www.w3.org/1999/xhtml">
        <head>
          <title>
	        <xsl:choose>
				<xsl:when test="identifier=&quot;LogRequestedOnNonExistingPath&quot;">
					Revision retrieval error
				</xsl:when>
				<xsl:when test="identifier=&quot;StatusOfNonExistingFile&quot;">
					Status error
				</xsl:when>
				<xsl:when test="identifier=&quot;AttachOnFile&quot;">
					Couldn't attach file
				</xsl:when>
				<xsl:when test="identifier=&quot;AttachOnNonExisting&quot;">
					Couldn't attach file
				</xsl:when>
				<xsl:when test="identifier=&quot;CommitOnNonExistingFile&quot;">
					Commit error
				</xsl:when>
			</xsl:choose>
          </title>
		      <xsl:element name="meta">
		           <xsl:attribute name="http-equiv">cache-control</xsl:attribute>
		           <xsl:attribute name="content">no-cache</xsl:attribute>
		      </xsl:element>
		      
		      <xsl:element name="meta">
		           <xsl:attribute name="http-equiv">pragma</xsl:attribute>
		           <xsl:attribute name="content">no-cache</xsl:attribute>
		      </xsl:element>
		      <xsl:call-template name="css_header"/>
        </head>
        <body>
        <div class="gwTopic">
	        <div class="gwError">
	        <xsl:choose>
				<xsl:when test="identifier=&quot;LogRequestedOnNonExistingPath&quot;">
					Revision can't be given on a non-existing path
				</xsl:when>
				<xsl:when test="identifier=&quot;StatusOfNonExistingFile&quot;">
					Can't request the status of a non-existing file or directory !
				</xsl:when>
				<xsl:when test="identifier=&quot;AttachOnFile&quot;">
					Files can't be attached to files, only to directories !
				</xsl:when>
				<xsl:when test="identifier=&quot;AttachOnNonExisting&quot;">
					Files can't be attached to unexisting directories !
				</xsl:when>
				<xsl:when test="identifier=&quot;CommitOnNonExistingFile&quot;">
					Commit can't be done on a non-existing file or directory !
				</xsl:when>
			</xsl:choose>
			</div>
        </div>
        </body>
        </html>
   </xsl:template>
    
</xsl:stylesheet>

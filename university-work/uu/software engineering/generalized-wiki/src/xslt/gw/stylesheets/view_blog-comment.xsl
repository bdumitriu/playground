<?xml version="1.0"?>

<!-- Stylesheet for viewing a individual comment or entry -->

<xsl:stylesheet version="1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:gw="http://www.cs.uu.nl/wiki/Gw"
  xmlns:xhtml="http://www.w3.org/1999/xhtml"
  xmlns:file="gw.blog.FileUtils"
  xmlns="http://www.w3.org/1999/xhtml"
  extension-element-prefixes="file"
  exclude-result-prefixes="gw xhtml">

  <xsl:import href="resource:/gw/stylesheets/blog-default.xsl"/>
  <xsl:include href="resource:/gw/stylesheets/lib/statusbar.xsl"/>

  <xsl:param name="pathinfo"/>
  
  <xsl:output method="xml" indent="yes"/>
  
  <xsl:template match="comment">
    <html>
      <head>
        <title><xsl:value-of select="$pathinfo"/></title>
        <xsl:call-template name="css_header"/>
      </head>
      <body>
        <xsl:call-template name="sidebar">
          <xsl:with-param name="path" select="file:dirname(file:dirname($pathinfo))"/>
        </xsl:call-template>
        <div class="gwTopic">
          <xsl:apply-imports/>
          <p><a href="/gw/view{file:dirname($pathinfo)}/entry.xml">&lt;&lt; Go to entry</a></p>
          <xsl:call-template name="statusbar"/>
        </div>
      </body>
    </html>
  </xsl:template>
  
</xsl:stylesheet>

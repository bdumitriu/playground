<?xml version="1.0"?>

<!-- Stylesheet for a chronological overview of all blog entries -->

<xsl:stylesheet version="1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:gw="http://www.cs.uu.nl/wiki/Gw"
  xmlns:xhtml="http://www.w3.org/1999/xhtml"
  xmlns="http://www.w3.org/1999/xhtml"
  xmlns:date="http://exslt.org/dates-and-times"
  xmlns:file="gw.blog.FileUtils"
  xmlns:blog="gw.blog.BlogUtils"
  extension-element-prefixes="file blog"
  exclude-result-prefixes="gw xhtml">
  
  <xsl:import href="resource:/gw/stylesheets/blog-default.xsl"/>
  <xsl:include href="resource:/gw/stylesheets/lib/statusbar.xsl"/>
  
  <xsl:param name="pathinfo"/>
  
  <xsl:variable name="normalizedPath" select="file:dirname($pathinfo)"/>
  <xsl:variable name="dirList" select="blog:mergeEntries($normalizedPath)"/>
  
  <xsl:output method="xml" indent="yes"/>
  
  <xsl:template match="blog">
    <html>
      <head>
        <title><xsl:value-of select="$pathinfo"/></title>    
        <xsl:call-template name="css_header"/>
      </head>
      <body>
        <xsl:call-template name="sidebar">
          <xsl:with-param name="path" select="$normalizedPath"/>
        </xsl:call-template>
        <div class="gwTopic">
          <xsl:apply-imports/>
          <hr/>
          <xsl:for-each select="$dirList/file">
            <xsl:sort select="entry/date" order="descending"/>
            <xsl:apply-templates select="entry">
              <xsl:with-param name="path" select="@path"/>
              <xsl:with-param name="comment-link" select="true()"/>
            </xsl:apply-templates>
          </xsl:for-each>
          <p><strong><a href="/gw/edit{concat($normalizedPath, '/', blog:uniqueId())}/entry.xml?content-type=blog/entry">Create new entry</a></strong></p>
          <xsl:call-template name="statusbar"/>
        </div>
       </body> 
    </html>
  </xsl:template>
  
</xsl:stylesheet>

<?xml version="1.0"?>

<!-- Stylesheet for viewing a blog entry -->

<xsl:stylesheet version="1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:gw="http://www.cs.uu.nl/wiki/Gw"
  xmlns:xhtml="http://www.w3.org/1999/xhtml"
  xmlns="http://www.w3.org/1999/xhtml"
  xmlns:file="gw.blog.FileUtils"
  xmlns:blog="gw.blog.BlogUtils"
  extension-element-prefixes="file blog"
  exclude-result-prefixes="gw xhtml">
  
  <xsl:import href="resource:/gw/stylesheets/blog-default.xsl"/>
  <xsl:include href="resource:/gw/stylesheets/lib/statusbar.xsl"/>
  
  <xsl:param name="pathinfo"/>
  
  <xsl:variable name="normalizedPath" select="file:dirname($pathinfo)"/>
  <xsl:variable name="dirList" select="blog:mergeComments($normalizedPath)"/>
  
  <xsl:output method="xml" indent="yes"/>
  
  <xsl:template match="entry">
    <html>
      <head>
        <title><xsl:value-of select="$pathinfo"/></title>    
        <xsl:call-template name="css_header"/>
      </head>
      <body>
        <xsl:call-template name="sidebar">
          <xsl:with-param name="path" select="file:dirname($normalizedPath)"/>
        </xsl:call-template>
        <div class="gwTopic">
          <xsl:apply-imports/>
          <hr/>
          <a name="comments"/>
          <h3><xsl:value-of select="count($dirList/file)"/> comments</h3>
          <xsl:for-each select="$dirList/file">
            <xsl:sort select="comment/date" order="ascending"/>
            <xsl:apply-templates select="comment">
              <xsl:with-param name="path" select="@path"/>
              <xsl:with-param name="edit-link" select="true()"/>
            </xsl:apply-templates>
          </xsl:for-each>
          <p><strong><a href="/gw/edit{concat($normalizedPath, '/', blog:uniqueId())}.xml?content-type=blog/comment">Post a comment</a></strong></p>        
          <p><a href="/gw/view{file:dirname($normalizedPath)}/index.xml">&lt;&lt; Go to index</a></p>
          <xsl:call-template name="statusbar"/>
        </div>
      </body> 
    </html>
  </xsl:template>
  
</xsl:stylesheet>

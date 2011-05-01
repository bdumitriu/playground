<?xml version="1.0"?>

<xsl:stylesheet version="1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:gw="http://www.cs.uu.nl/wiki/Gw"
  xmlns:xhtml="http://www.w3.org/1999/xhtml"
  xmlns="http://www.w3.org/1999/xhtml"
  xmlns:file="gw.blog.FileUtils"
  xmlns:blog="gw.blog.BlogUtils"
  exclude-result-prefixes="gw xhtml"
  extension-element-prefixes="file blog">
  
  <xsl:import href="resource:/gw/stylesheets/blog-default.xsl"/>
  
  <xsl:param name="pathinfo"/>

  <xsl:output method="xml" indent="yes"/>
  
  <xsl:template match="editForm">
    <html>
      <head>
        <title>Edit <xsl:value-of select="$pathinfo"/></title>
        <xsl:call-template name="css_header"/>
      </head>
      <body class="gwEditPage">
        <xsl:call-template name="sidebar">
          <xsl:with-param name="path" select="file:dirname(file:dirname($pathinfo))"/>
        </xsl:call-template>
        <div class="gwTopic">
          <h1>Edit comment</h1>
          <xsl:call-template name="commentEditForm"/>
        </div>
      </body>
    </html>
  </xsl:template>
  
  <xsl:template name="commentEditForm">
    <form class="gwEditForm" action="/gw/blog/preview-comment{$pathinfo}" method="post">
      <p>
        <textarea name="entry" rows="35" cols="100">
          <xsl:value-of select="blog:xmlToString(parsecontent/comment/content/*)"/>
        </textarea><br/>
        
        ContentType: <input type="text" name="contenttype">
          <xsl:choose>
            <xsl:when test="parsecontent/comment/content/@contenttype">
              <xsl:attribute name="value"><xsl:value-of select="parsecontent/comment/content/@contenttype"/></xsl:attribute>
            </xsl:when>
            <xsl:otherwise>
              <xsl:attribute name="value">application/gwml+xml</xsl:attribute>
            </xsl:otherwise>
          </xsl:choose>        
        </input>
      </p>
      <p>
        <input name="btnPreview" type="submit" value="Preview"/>
        <a href="/gw/view{$pathinfo}">Cancel this edit</a>
      </p>
    </form>
  </xsl:template>

</xsl:stylesheet>

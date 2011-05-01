<?xml version="1.0"?>

<xsl:stylesheet version="1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:gw="http://www.cs.uu.nl/wiki/Gw"
  xmlns:xhtml="http://www.w3.org/1999/xhtml"
  xmlns="http://www.w3.org/1999/xhtml"
  xmlns:blog="gw.blog.BlogUtils"
  xmlns:file="gw.blog.FileUtils"
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
          <h1>Edit entry</h1>
          <xsl:call-template name="entryEditForm"/>
        </div>
      </body>
    </html>
  </xsl:template>
  
  <xsl:template name="entryEditForm">
    <form class="gwEditForm" action="/gw/blog/preview-entry{$pathinfo}" method="post">
      <p>Title:
        <input name="title" size="70" value="{parsecontent/entry/title}"/><br/>
        
        <textarea name="entry" rows="35" cols="100">
          <xsl:value-of select="blog:xmlToString(parsecontent/entry/content/*)"/>
        </textarea><br/>
        
        ContentType: <input type="text" name="contenttype">
          <xsl:choose>
            <xsl:when test="parsecontent/entry/content/@contenttype">
              <xsl:attribute name="value"><xsl:value-of select="parsecontent/entry/content/@contenttype"/></xsl:attribute>
            </xsl:when>
            <xsl:otherwise>
              <xsl:attribute name="value">application/gwml+xml</xsl:attribute>
            </xsl:otherwise>
          </xsl:choose>        
        </input><br/>
        
        <input id="check_draft" type="checkbox" name="draft" value="yes">
          <xsl:if test="parsecontent/entry[@draft = 'yes']">
            <xsl:attribute name="checked">checked</xsl:attribute>
          </xsl:if>
        </input>
        <label for="check_draft">Save as draft</label><br/>
      </p>
      <p>
        <input name="btnPreview" type="submit" value="Preview"/>
        <a href="/gw/view{$pathinfo}">Cancel this edit</a>
      </p>
    </form>
  </xsl:template>

</xsl:stylesheet>

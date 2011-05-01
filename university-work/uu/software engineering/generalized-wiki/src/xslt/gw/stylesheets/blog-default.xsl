<?xml version="1.0"?>

<xsl:stylesheet version="1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:gw="http://www.cs.uu.nl/wiki/Gw"
  xmlns:xhtml="http://www.w3.org/1999/xhtml"
  xmlns="http://www.w3.org/1999/xhtml"
  xmlns:date="http://exslt.org/dates-and-times"
  xmlns:file="gw.blog.FileUtils"
  xmlns:blog="gw.blog.BlogUtils"
  extension-element-prefixes="date file blog"
  exclude-result-prefixes="gw xhtml">
  
  <xsl:import href="resource:/gw/stylesheets/lib/master.xsl"/>
  
  
  <xsl:template match="date">
    <xsl:value-of select="date:year(.)"/>
    <xsl:text>-</xsl:text>
    <xsl:value-of select="date:month-in-year(.)"/>
    <xsl:text>-</xsl:text>
    <xsl:value-of select="date:day-in-month(.)"/>
    <xsl:text> </xsl:text>
    <xsl:value-of select="date:hour-in-day(.)"/>
    <xsl:text>:</xsl:text>
    <xsl:value-of select="date:minute-in-hour(.)"/>
  </xsl:template>


  <xsl:template name="sidebar">
    <xsl:param name="path"/>
    
    <div class="gwLeftBar">
      <strong>Latest entries</strong><br/>
      <br/>
      <xsl:variable name="dirList" select="blog:mergeEntries($path)"/>
      <xsl:for-each select="$dirList/file[not(entry/@draft = 'yes')]">
        <xsl:sort select="entry/date" order="descending"/>
        <xsl:if test="position() &lt;= 10">
          <a href="/gw/view{@path}"><xsl:value-of select="entry/title"/></a>
          <br/>
        </xsl:if> 
      </xsl:for-each>
    </div>
  </xsl:template>
  
  
  <xsl:template match="blog">
    <h1><xsl:value-of select="title"/></h1>
    <p><xsl:apply-templates select="description"/></p>
  </xsl:template>  
  
  
  <xsl:template match="entry">
    <xsl:param name="path" select="$pathinfo"/>
    <xsl:param name="comment-link" select="false()"/>
    <xsl:param name="edit-link" select="false()"/>
    
    <div class="blogEntry">
      <xsl:if test="@draft = 'yes'">
        <xsl:attribute name="style">font-style: italic;</xsl:attribute>
      </xsl:if>
      <h2><xsl:value-of select="title"/><xsl:if test="@draft = 'yes'"> (Draft)</xsl:if></h2>
      <p><xsl:apply-templates select="content"/></p>
      <p>Posted on <a href="/gw/view{$path}"><xsl:apply-templates select="date"/></a> by <xsl:value-of select="author"/>
        <xsl:if test="$comment-link">
          <xsl:text> | </xsl:text>
          <a href="/gw/view{$path}#comments"><xsl:value-of select="count(blog:mergeComments(file:dirname($path))/file/comment)"/> comments</a>
        </xsl:if>
        <xsl:if test="$edit-link">
          <xsl:text> | </xsl:text>
          <small><a href="/gw/edit{$path}">edit</a></small>
          <xsl:text> - </xsl:text>
          <small><a href="/gw/delete{$path}">delete</a></small>      
        </xsl:if>
      </p>
    </div> 
  </xsl:template>
  
  
  <xsl:template match="comment">
    <xsl:param name="path" select="$pathinfo"/>
    <xsl:param name="edit-link" select="false()"/>
    
    <div class="blogComment">
      <p><xsl:apply-templates select="content"/></p>
      <p>Posted on <a href="/gw/view{$path}"><xsl:apply-templates select="date"/></a> by <xsl:value-of select="author"/>
        <xsl:if test="$edit-link">
          <xsl:text> | </xsl:text>
          <small><a href="/gw/edit{$path}">edit</a></small>
          <xsl:text> - </xsl:text>
          <small><a href="/gw/delete{$path}">delete</a></small>
        </xsl:if>
      </p>
    </div>
  </xsl:template>
  
  <xsl:template match="exception">
    <span style="color: red;"><xsl:apply-templates/></span>
  </xsl:template>
    
</xsl:stylesheet>

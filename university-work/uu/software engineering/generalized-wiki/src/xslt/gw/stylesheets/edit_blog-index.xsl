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
  
  <xsl:param name="pathinfo"/>
  
  <xsl:variable name="normalizedPath" select="file:dirname($pathinfo)"/>
  <xsl:variable name="dirList" select="blog:mergeEntries($normalizedPath)"/>
  
  <xsl:output method="xml" indent="yes"/>
  
  <xsl:template match="editForm">
    <html>
      <head>
        <title>Edit <xsl:value-of select="$pathinfo"/></title>
        <xsl:call-template name="css_header"/>
      </head>
      <body class="gwEditPage">
        <xsl:call-template name="sidebar">
          <xsl:with-param name="path" select="$normalizedPath"/>
        </xsl:call-template>
        <div class="gwTopic">
          <h1>Edit blog</h1>
          <h3>Settings</h3>
          <xsl:call-template name="blogEditForm"/>
          <hr/>
          <h3>Entries</h3>
          <table>
            <tr>
              <th>Date</th>
              <th/>
              <th>Title</th>
              <th/>
              <th/>
            </tr>
            <xsl:for-each select="$dirList/file">
              <xsl:sort select="entry/date" order="descending"/>
              <xsl:apply-templates select="entry">
                <xsl:with-param name="path" select="@path"/>
              </xsl:apply-templates>
            </xsl:for-each>
          </table>
          <p><strong><a href="/gw/edit{concat($normalizedPath, '/', blog:uniqueId())}/entry.xml?content-type=blog/entry">Create new entry</a></strong></p>
        </div>
      </body>
    </html>
  </xsl:template>
  
  <xsl:template match="entry">
    <xsl:param name="path"/>
    
    <tr>
      <td><xsl:apply-templates select="date"/></td>
      <td><a href="/gw/edit{$path}">Edit</a></td>
      <td><xsl:value-of select="title"/></td>
      <td>
        <xsl:choose>
          <xsl:when test="@draft = 'yes'">
            <em>Draft</em>
          </xsl:when>
          <xsl:otherwise>
            <a href="/gw/view{$path}">View</a>
          </xsl:otherwise>
        </xsl:choose>
      </td>
      <td><a href="/gw/delete{$path}">Delete</a></td>
    </tr>
  </xsl:template>
  
  <xsl:template name="blogEditForm">
    <form class="gwEditForm" action="/gw/blog/preview{$pathinfo}" method="post">
      <p>Title: <input name="title" size="70" value="{parsecontent/blog/title}"/><br/>
        Description:<br/>
        <textarea name="description" rows="5" cols="100">
          <xsl:value-of select="parsecontent/blog/description"/>
        </textarea><br/>
      </p>
      <p>
        <input name="btnPreview" type="submit" value="Preview"/>
        <a href="/gw/view{$pathinfo}">Cancel this edit</a>
      </p>
    </form>  
  </xsl:template>
  
</xsl:stylesheet>

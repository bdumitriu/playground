<?xml version="1.0"?>

<xsl:stylesheet version="1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:gw="http://www.cs.uu.nl/wiki/Gw"
  xmlns:xhtml="http://www.w3.org/1999/xhtml"
  xmlns="http://www.w3.org/1999/xhtml"
  xmlns:file="gw.blog.FileUtils"
  exclude-result-prefixes="gw xhtml"
  extension-element-prefixes="file">
  
  <xsl:import href="resource:/gw/stylesheets/blog-default.xsl"/>
  <xsl:include href="resource:/gw/stylesheets/lib/preview-form.xsl"/>
 
  <xsl:output method="xml" indent="yes"/>
  <xsl:param name="pathinfo"/> 

  <xsl:template match="/">
    <html>
      <head>
        <title>Preview of <xsl:value-of select="$pathinfo"/></title>
        <meta http-equiv="Pragma" content="no-cache"/>
        <meta http-equiv="Expires" content="-1"/>
        <xsl:call-template name="css_header"/>
      </head>
      <body class="gwPreviewPage">
        <xsl:call-template name="sidebar">
          <xsl:with-param name="path" select="file:dirname($pathinfo)"/>
        </xsl:call-template>
        <div class="gwTopic">
          <h1>This is a PREVIEW of your blog</h1>
          <div class="gwPreviewArea">
            <xsl:apply-templates/>
          </div>
          <xsl:call-template name="previewForm"/>
        </div>
      </body>
    </html>
  </xsl:template>
  
</xsl:stylesheet>

<?xml version="1.0"?>
<xsl:stylesheet version="1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:gw="http://www.cs.uu.nl/wiki/Gw"
  xmlns:xhtml="http://www.w3.org/1999/xhtml"
  xmlns:date="http://exslt.org/dates-and-times"
  xmlns="http://www.w3.org/1999/xhtml"
  exclude-result-prefixes="gw xhtml"
  extension-element-prefixes="date">

  <xsl:output method="html" indent="yes"/>
  <xsl:import href="resource:/gw/stylesheets/lib/master.xsl"/>

  <xsl:template match="log">
    <html>
      <head>
        <title>Subversion log</title>
        <xsl:call-template name="css_header"/>
        <link rel="stylesheet" type="text/css" media="screen" href="/gw/view/trunk/svnlog/svnlog.css"/>
      </head>
      <body>
        <div class="gwTopic">
          <xsl:apply-templates/>
        </div>
      </body>
    </html>
  </xsl:template>

  <xsl:template match="logEntry">
    <div class="logentry">
      <p>
        <a name="{@revision}"/>
        <img src="/gw/view/trunk/svnlog/page_number_24.gif" align="middle"/>
        <a href="#{@revision}" title="Link to this commit"><xsl:value-of select="@revision"/></a>
        <img src="/gw/view/trunk/svnlog/calendar_24.gif" align="middle" style="margin-left: 15pt"/>
        <xsl:value-of select="date:year(date/text())"/>-<xsl:value-of select="date:month-in-year(date/text())"/>-<xsl:value-of select="date:day-in-month(date/text())"/>
         @ <xsl:value-of select="date:hour-in-day(date/text())"/>:<xsl:value-of select="date:minute-in-hour(date/text())"/>
        <img src="/gw/view/trunk/svnlog/user_24.gif" align="middle" style="margin-left: 15pt"/>
        <xsl:value-of select="author"/>
      </p>

      <div class="commitmsg">
        <pre width="85">
          <code>
            <xsl:value-of select="msg"/>
          </code>
        </pre>
      </div>
      
      <table border="0" cellspacing="5">
        <xsl:apply-templates select="paths"/>
      </table>
    </div>
  </xsl:template>

  <xsl:template match ="paths">
    <xsl:apply-templates select="path"/>
  </xsl:template>

  <xsl:template match="path">
    <xsl:variable name="action" select="@action"/>
    
    <tr>
    <td>
    <xsl:choose>
     <xsl:when test="$action=&quot;M&quot;">
       <img src="/gw/view/trunk/svnlog/edit_16.gif"/>
     </xsl:when>
     <xsl:when test="$action=&quot;D&quot;">
       <img src="/gw/view/trunk/svnlog/delete_16.gif"/>
     </xsl:when>
     <xsl:when test="$action=&quot;A&quot;">
       <img src="/gw/view/trunk/svnlog/plus_16.gif"/>
     </xsl:when>
    </xsl:choose>
    </td>
    <td><xsl:value-of select="."/></td>
    <td>
     <a href="https://svn.cs.uu.nl:12443/repos/test-wiki{.}" title="Open file">
     <img border="0" src="/gw/view/trunk/svnlog/open_16.gif"/></a>
    </td>
    <td>
      <a href="https://svn.cs.uu.nl:12443/viewcvs/test-wiki{.}" title="Open file in ViewCVS">
      <img border="0" src="/gw/view/trunk/svnlog/zoom_16.gif"/></a>
    </td>
    <td>
    <xsl:choose>
     <xsl:when test="$action=&quot;M&quot;">
       <a href="https://svn.cs.uu.nl:12443/viewcvs/test-wiki{.}?r1={(../../@revision)-1}&amp;r2={../../@revision}&amp;p1={.}&amp;p2={.}"
             title="View difference to previous revision in ViewCVS">
       <img border="0" src="/gw/view/trunk/svnlog/window_tile_vert_16.gif"/></a>
     </xsl:when>
    </xsl:choose>
    </td>
    </tr>
  </xsl:template>
</xsl:stylesheet>

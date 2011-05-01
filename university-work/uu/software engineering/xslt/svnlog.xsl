<xsl:stylesheet
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    version="1.0"
    xmlns:date="http://exslt.org/dates-and-times"
    extension-element-prefixes="date">

  <xsl:output method="html" indent="yes" />

  <xsl:template match="log">
    <html>
      <head>
	<title>Subversion Log</title>
	<link rel="stylesheet" type="text/css" href="svnlog/svnlog.css" />
      </head>
      <body>
	<xsl:apply-templates />
      </body>
    </html>
  </xsl:template>

  <xsl:template match="logentry">
    <div class="logentry">
      <p>
	<a name="{@revision}" />
	<img src="svnlog/page_number_24.gif" align="middle" />
	<a href="#{@revision}" title="Link to this commit"><xsl:value-of select="@revision" /></a>
	<img src="svnlog/calendar_24.gif" align="middle" style="margin-left: 15pt" />
	<xsl:value-of select="date:year(date/text())"/>
	<xsl:text>-</xsl:text>
	<xsl:value-of select="date:month-in-year(date/text())" />
	<xsl:text>-</xsl:text>
	<xsl:value-of select="date:day-in-month(date/text())" />
	<xsl:text> </xsl:text>
	<xsl:value-of select="date:hour-in-day(date/text())" />
	<xsl:text>:</xsl:text>
	<xsl:value-of select="date:minute-in-hour(date/text())" />
	<img src="svnlog/user_24.gif" align="middle" style="margin-left: 15pt" />
	<xsl:value-of select="author" />
      </p>

      <div class="commitmsg">
	<pre width="85">
	  <code>
	    <xsl:value-of select="msg" />
          </code>
	</pre>
      </div>
      
      <table border="0" cellspacing="5">
	<i><xsl:apply-templates /></i>
      </table>
    </div>
  </xsl:template>

  <xsl:template match="date" />
  <xsl:template match="author" />
  <xsl:template match="msg" />

  <xsl:template match="path[@action = 'M']">
    <xsl:variable name="path" select="text()" />
    <xsl:variable name="thisrevision" select="ancestor::logentry/@revision" />
    <xsl:variable name="previousrevision" select="($thisrevision - 1)" />
    <tr>
      <td><img src="svnlog/edit_16.gif" title="Modified"/></td>

      <td><xsl:value-of select="$path" /></td>

      <td>
        <a href="https://svn.cs.uu.nl:12443/repos/test-wiki{$path}" title="Open file">
	  <img border="0" src="svnlog/open_16.gif"/>
	</a>
      </td>

      <td>
	<a href="https://svn.cs.uu.nl:12443/viewcvs/test-wiki{$path}" title="Open file in ViewCVS">
	  <img border="0" src="svnlog/zoom_16.gif"/>
	</a>
      </td>

      <td>
	<a
	    href="https://svn.cs.uu.nl:12443/viewcvs/test-wiki{$path}?r1={$previousrevision}&amp;r2={$thisrevision}&amp;p1={$path}&amp;p2={$path}"
	    title="View difference to previous revision in ViewCVS">
	  <img border="0" src="svnlog/window_tile_vert_16.gif"/>
	</a>
      </td>
    </tr>
  </xsl:template>

  <xsl:template match="path[@action = 'A']">
    <xsl:variable name="path" select="text()" />
    <xsl:variable name="thisrevision" select="ancestor::logentry/@revision" />
    <xsl:variable name="previousrevision" select="($thisrevision - 1)" />
    <tr>
      <td><img src="svnlog/plus_16.gif" title="Added"/></td>

      <td><xsl:value-of select="$path" /></td>

      <td>
        <a href="https://svn.cs.uu.nl:12443/repos/test-wiki{$path}" title="Open file">
	  <img border="0" src="svnlog/open_16.gif"/>
	</a>
      </td>

      <td>
	<a href="https://svn.cs.uu.nl:12443/viewcvs/test-wiki{$path}" title="Open file in ViewCVS">
	  <img border="0" src="svnlog/zoom_16.gif"/>
	</a>
      </td>
    </tr>
  </xsl:template>

  <xsl:template match="path[@action = 'D']">
    <xsl:variable name="path" select="text()" />
    <xsl:variable name="thisrevision" select="ancestor::logentry/@revision" />
    <xsl:variable name="previousrevision" select="($thisrevision - 1)" />
    <tr>
      <td><img src="svnlog/delete_16.gif" title="Deleted"/></td>

      <td><xsl:value-of select="$path" /></td>

      <td>
        <a href="https://svn.cs.uu.nl:12443/repos/test-wiki{$path}" title="Open file">
	  <img border="0" src="svnlog/open_16.gif"/>
	</a>
      </td>

      <td>
	<a href="https://svn.cs.uu.nl:12443/viewcvs/test-wiki{$path}" title="Open file in ViewCVS">
	  <img border="0" src="svnlog/zoom_16.gif"/>
	</a>
      </td>
    </tr>
  </xsl:template>

<!-- This was the first implementation, making use of various control flow tags -->
<!--
  <xsl:template match="path">
    <xsl:variable name="path" select="text()" />
    <xsl:variable name="thisrevision" select="ancestor::logentry/@revision" />
    <xsl:variable name="previousrevision" select="($thisrevision - 1)" />
    <tr>
      <td>
	<xsl:choose>
	  <xsl:when test="@action = 'M'"><img src="svnlog/edit_16.gif" title="Modified"/></xsl:when>
	  <xsl:when test="@action = 'A'"><img src="svnlog/plus_16.gif" title="Added"/></xsl:when>
	  <xsl:when test="@action = 'D'"><img src="svnlog/delete_16.gif" title="Deleted"/></xsl:when>
	  <xsl:otherwise></xsl:otherwise>
	</xsl:choose>
      </td>

      <td><xsl:value-of select="$path" /></td>

      <td>
        <a href="https://svn.cs.uu.nl:12443/repos/test-wiki{$path}" title="Open file">
	  <img border="0" src="svnlog/open_16.gif"/>
	</a>
      </td>

      <td>
	<a href="https://svn.cs.uu.nl:12443/viewcvs/test-wiki{$path}" title="Open file in ViewCVS">
	  <img border="0" src="svnlog/zoom_16.gif"/>
	</a>
      </td>

      <xsl:if test="@action = 'M'">
	<td>
	  <a
	      href="https://svn.cs.uu.nl:12443/viewcvs/test-wiki{$path}?r1={$previousrevision}&amp;r2={$thisrevision}&amp;p1={$path}&amp;p2={$path}"
	      title="View difference to previous revision in ViewCVS">
	    <img border="0" src="svnlog/window_tile_vert_16.gif"/>
	  </a>
      </td>	
      </xsl:if>

    </tr>
  </xsl:template>
-->


</xsl:stylesheet>

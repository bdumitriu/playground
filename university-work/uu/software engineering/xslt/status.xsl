<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

  <xsl:output method="html" indent="yes" />

  <xsl:template match="status">
    <html>
      <head>
	<title>Subversion Status</title>
      </head>
      <body>
	<table cellspacing="10">
	  <tr>
	    <th>Path</th>
	    <th>Changes</th>
	    <th>Working Revision</th>
	    <th>Last Committed Revision</th>
	    <th>Last Committed Author</th>
	  </tr>
	  <xsl:apply-templates />
	</table>
      </body>
    </html>
  </xsl:template>

  <xsl:template match="file">
    <tr>
      <td><xsl:value-of select="@path" /></td>
      <xsl:apply-templates />
    </tr>
  </xsl:template>

  <xsl:template match="changes">
    <td><xsl:apply-templates /></td>
  </xsl:template>

  <xsl:template match="working-revision">
    <td><xsl:apply-templates /></td>
  </xsl:template>

  <xsl:template match="revision">
    <td><xsl:apply-templates /></td>
  </xsl:template>

  <xsl:template match="author">
    <td><xsl:apply-templates /></td>
  </xsl:template>

  <xsl:template match="modified">M</xsl:template>
  <xsl:template match="added">A</xsl:template>
  <xsl:template match="deleted">D</xsl:template>

</xsl:stylesheet>

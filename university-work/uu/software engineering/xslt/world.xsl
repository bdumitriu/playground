<?xml version="1.0"?>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <xsl:output method="html" indent="yes" />

  <xsl:template match="hello">

    <html>
      <head>
	<title>Hello <xsl:value-of select="." /></title>
      </head>
      <body>
	<h1>Hello <xsl:value-of select="." /></h1>
      </body>
    </html>

  </xsl:template>

</xsl:stylesheet>

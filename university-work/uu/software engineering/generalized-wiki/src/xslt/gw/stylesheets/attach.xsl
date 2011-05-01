<?xml version="1.0"?>

<xsl:stylesheet version="1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:gw="http://www.cs.uu.nl/wiki/Gw"
  xmlns:xhtml="http://www.w3.org/1999/xhtml"
  xmlns="http://www.w3.org/1999/xhtml"
  exclude-result-prefixes="gw xhtml"
  xmlns:xalan="http://xml.apache.org/xalan"
  xmlns:extFunct="gw.render.extensions.ExtensionFunctions"
  extension-element-prefixes="extFunct xalan">

  <xsl:import href="resource:/gw/stylesheets/lib/master.xsl" />
  <xsl:param name="pathinfo"/>
  
  <xsl:output method="xml" media-type="application/xhtml+xml"/>
  
  <xsl:template match="empty">
    <html xmlns="http://www.w3.org/1999/xhtml">
  <head>
    <title>Attach</title>
    <meta http-equiv="cache-control" content="no-cache" />
    <meta http-equiv="pragma" content="no-cache" />
    <xsl:call-template name="css_header"/>
  </head>
  <body>
  <div class="gwTopic">
  	<h1>Attach file(s)</h1>
    Select the file(s) you want to upload to <xsl:value-of select="$pathinfo"/>. Files bigger than  1024kB are ignored! <br />
    <form method="POST" enctype="multipart/form-data" action="{concat(extFunct:getUrl('attach', $pathinfo),@href)}">
      <input type="file" size ="40" name="file0" /><br /><input type="submit" value="Upload"/><br />
    </form>
  </div>
  </body>
</html>
  </xsl:template>
</xsl:stylesheet>

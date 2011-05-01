<?xml version="1.0"?>
<xsl:stylesheet version="1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns="http://www.w3.org/1999/xhtml"
  xmlns:xalan="http://xml.apache.org/xalan"
  xmlns:extFunct="gw.render.extensions.ExtensionFunctions"
  extension-element-prefixes="extFunct xalan"
  >

  <xsl:template name="previewForm">   
    <hr/>

    <form action="{extFunct:getUrl('save', $pathinfo)}" method="post">     
      <input type="hidden" name="text" value="{extFunct:getFileContent($pathinfo)}" />
      <input type="hidden" name="contenttype" value="{extFunct:getContentType($pathinfo)}" />
      <input type="hidden" name="acl" value="{extFunct:getAcl($pathinfo)}"/>

      <p>
	I want to commit right away <input type="checkbox" name="commit" />
      </p>

      <p>
	<input type="submit" value="Save this file" />
      </p>
    </form>
  </xsl:template>
</xsl:stylesheet>

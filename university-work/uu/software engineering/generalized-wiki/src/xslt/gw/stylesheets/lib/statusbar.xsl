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
    
  <xsl:template name="statusbar">
    <!-- SVN status -->
    <hr />
    <xsl:apply-templates select="extFunct:getFileStatus($pathinfo)"/>
    
    <!-- Edit bar -->
    <hr />
    <a href="{extFunct:getUrl('view', '/trunk/WebHome')}">Trunk/WebHome</a>
    | <a href="{extFunct:getUrl('edit', $pathinfo)}">Edit</a>
    | <a href="{extFunct:getUrl('move', $pathinfo)}">Move</a>
    | <a href="{extFunct:getUrl('copy', $pathinfo)}">Copy</a>
    | <a href="{extFunct:getUrl('revert', $pathinfo)}">Revert</a>
    | <a href="{extFunct:getUrl('acl', $pathinfo)}">ACL</a>
    | <a href="{extFunct:getUrl('revision', $pathinfo)}">ChangeLog</a>
    | <a href="{extFunct:getUrl('delete', $pathinfo)}">Delete</a>
    | <a href="{extFunct:getUrl('status', $pathinfo)}">Status</a>
    | <a href="{extFunct:getUrl('commit', $pathinfo)}">Commit</a> 
    | <a href="{extFunct:getUrl('attach', $pathinfo)}">Attach</a>    
    | <a href="{extFunct:getPrefix()}newuser/">NewUser</a>
    | <a href="{extFunct:getPrefix()}conference/newConference{$pathinfo}">Make conference</a>
    | <a href="{extFunct:getPrefix()}login/">Login</a>
    | <a href="{extFunct:getPrefix()}logout/">Logout</a>
    | <a href="{extFunct:getPrefix()}resetpassword/">ResetPW</a>
    | <a href="{extFunct:getPrefix()}search/statistics">Stats</a>
    Broken:  
    <a href="{extFunct:getUrl('diff', $pathinfo)}">Diff</a>
    | <a href="{extFunct:getUrl('praise', $pathinfo)}">Praise</a>
    | <a href="{extFunct:getPrefix()}setpassword/">SetPassword</a>
    | <a href="{extFunct:getPrefix()}search/">Search</a>
    | <a href="{extFunct:getPrefix()}search/rebuildIndex">RebuildIndex</a>    
  </xsl:template>

  <xsl:template match="status">
  <span class="gwRevInfo">
    <a href="{extFunct:getUrl('revision', $pathinfo)}">
    	<xsl:value-of select="revision/text()" />
    </a>
    -
    <xsl:value-of select="author/text()"/>
    -
    <xsl:value-of select="date/text()" />
    -
    SVN status: <xsl:value-of select="svnstatus/text()" />
  </span>
  </xsl:template>
</xsl:stylesheet>

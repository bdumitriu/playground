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

  <xsl:param name="pathinfo"/>
  
  <xsl:import href="resource:/gw/stylesheets/lib/master.xsl" />
  <xsl:output method="html" media-type="application/xhtml+xml"/>

  <xsl:template match ="fileStatuses">
	  <html xmlns="http://www.w3.org/1999/xhtml">
	      <head>
	          <xsl:call-template name="css_header"/>
	          <title>Status</title>
			      <xsl:element name="meta">
			           <xsl:attribute name="http-equiv">cache-control</xsl:attribute>
			           <xsl:attribute name="content">no-cache</xsl:attribute>
			      </xsl:element>
			      
			      <xsl:element name="meta">
			           <xsl:attribute name="http-equiv">pragma</xsl:attribute>
			           <xsl:attribute name="content">no-cache</xsl:attribute>
			      </xsl:element>
	        </head>
	        <body>
	        <div class="gwTopic">   
	      <p>
	      <table>
	         <tr><th>Type</th><th>File/Directory</th><th>View</th><th>Status</th><th>Last Revision</th>
	         <th>Last commit author</th><th>Last change date</th></tr>
	         <tr><td colspan="4"></td></tr>
	                <xsl:for-each select="status">          
	                   <tr>
	                   <td>
	                       <xsl:choose>
					         <xsl:when test="dirOrFile=&quot;D&quot;">
					           Dir
					         </xsl:when>
					         <xsl:when test="dirOrFile=&quot;F&quot;">
					           File
					         </xsl:when>
					    </xsl:choose></td>
	                   <td><a href="{extFunct:getUrl('edit', filename)}"><xsl:value-of select="filename"/></a></td>
	                   <td>
	                   (<a href="{extFunct:getUrl('view', filename)}">View</a>)               
	                   </td>
					   <td><xsl:value-of select="statusDescription"/></td>
	                   <td><xsl:value-of select="lastRevisionNr"/></td>
	                   <td><xsl:value-of select="lastCommitAuthor"/></td>
	                   <td><xsl:value-of select="lastChangedDate"/></td>
	                   </tr>
	
		     </xsl:for-each> 
		     </table></p>
		     <a href="{extFunct:getUrl('commit', $pathinfo)}">Commit</a>
		</div>
        </body>
    </html>
      </xsl:template>
    
</xsl:stylesheet>
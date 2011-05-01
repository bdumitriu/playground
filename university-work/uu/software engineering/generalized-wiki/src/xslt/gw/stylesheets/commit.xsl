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

   <xsl:template match ="committingFiles">
        <html xmlns="http://www.w3.org/1999/xhtml">
        <head>
          <title>Commit</title>
          <xsl:call-template name="css_header"/>
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
      <xsl:if test="error">
        <h1><xsl:value-of select="error"/></h1>
      </xsl:if>
      
      <xsl:apply-templates select="mergeConflicts"/>

      <xsl:apply-templates select="fileStatuses"/>
      
      <form id="frmCommit" action="{extFunct:getUrl('commit', $pathinfo)}" method="post">

      Please enter a commit message
      <br />
      <textarea id="txtCommit" name="logmsg" rows="8" cols="40">
      </textarea>
      <br />      
        <input id="btnCommit" type="submit" value="Commit" />                  
      </form> 
      	</div>
        </body>
        </html>
   </xsl:template>
  
  
  <xsl:template match ="mergeConflicts">
   <xsl:choose>
  	<xsl:when test="conflict">
  	  <p>
  	  Warning !
  	  <xsl:element name="form">
          	<xsl:attribute name="action"><xsl:value-of select="contextPath"/>/resolved/</xsl:attribute>
		<xsl:attribute name="method">post</xsl:attribute>
          	<table><tr><th>Conflict Table</th><th></th><th></th></tr>
  		<xsl:for-each select="conflict">
  			<tr><td>
  			<xsl:element name="input">
		          	<xsl:attribute name="type">checkbox</xsl:attribute>
				<xsl:attribute name="name"><xsl:value-of select="fileName"/></xsl:attribute>
		        </xsl:element>
  			</td><td><xsl:value-of select="fileName"/>
  			</td><td>
  			<xsl:element name="a">
		          	<xsl:attribute name="href">
		          		<xsl:value-of select="../contextPath"/>/edit/<xsl:value-of select="fileName"/>
		          	</xsl:attribute>
		          	Edit
		        </xsl:element>
  			</td><td>
  			<xsl:element name="a">
		          	<xsl:attribute name="href">
		          		<xsl:value-of select="../contextPath"/>/revert/<xsl:value-of select="fileName"/>
		          	</xsl:attribute>
		          	Revert
		        </xsl:element>
		        </td></tr>
  		</xsl:for-each>
  		</table>
  		<xsl:element name="input">
			<xsl:attribute name="type">submit</xsl:attribute>
			<xsl:attribute name="value">Resolve!</xsl:attribute>
		</xsl:element>
	  </xsl:element>
  	  </p>
  	</xsl:when>
  	<xsl:otherwise>There were no conflicts</xsl:otherwise>
  </xsl:choose>  	
   
  </xsl:template>

  <xsl:template match ="fileStatuses">
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
      </xsl:template>
    
</xsl:stylesheet>

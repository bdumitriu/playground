<xsl:stylesheet version="1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:gw="http://www.cs.uu.nl/wiki/Gw"
  xmlns:xhtml="http://www.w3.org/1999/xhtml"
  xmlns="http://www.w3.org/1999/xhtml"
  exclude-result-prefixes="gw xhtml"
  
  xmlns:xalan="http://xml.apache.org/xalan"
  xmlns:repep="gw.render.extensions.ExtensionFunctions"
  extension-element-prefixes="repep xalan"
  >
  	<xsl:import href="resource:/gw/stylesheets/lib/master.xsl" />
  	<xsl:include href="resource:/gw/stylesheets/lib/statusbar.xsl" />
  
  	<xsl:output method="html" indent="yes"/>
  	<xsl:param name="pathinfo"/>
  	  
	<xsl:template match="conference">
		<html>
			<head>
				<xsl:call-template name="css_header"/>
			</head>
			
			<body>
			<div class="gwLeftBar">This is a leftbar</div>
			<div class="gwTopic">
		         <table> 
		            <xsl:apply-templates select="*">
		              <xsl:sort data-type="number" select="round( sum(./rater/rating/@value) div count(./rater/rating/@value) )" /> 
		            </xsl:apply-templates>
		         </table>
			     <!--Include-->
				<xsl:if test="//include">
					<xsl:call-template name="include"/>
				</xsl:if>		
				
				<!--xsl:value-of select="repep:include($pathinfo, 'text:html')"/-->
				
				<xsl:call-template name="statusbar"/>
			</div>
		    </body>
	     </html>
	
	</xsl:template>
	
	<xsl:template match="paper">
		  <tr> <th> <xsl:text>Paper name:</xsl:text> </th> <th> <xsl:text>Average score:</xsl:text> </th> <th> <xsl:text>Status</xsl:text> </th> <th> <xsl:text>Options</xsl:text> </th> </tr>
		  <tr> <td> <xsl:value-of select="@name"/> </td> 
	           <td> <xsl:value-of select="round( sum(./rater/rating/@value) div count(./rater/rating/@value) )"/> </td>
	      	   <td> <xsl:value-of select="@status"/> </td>
	      	   <td> </td>	 
	      </tr>
	      
		  <!-- <xsl:apply-templates select="*">
	      	<xsl:sort data-type="text" select="translate(@user, 
	                                                                 'ABCDEFGHIJKLMNOPQRSTUVWXYZ', 
	                                                                 'abcdefghijklmnopqrstuvwxyz')" />
	      </xsl:apply-templates> -->

	</xsl:template>
	
	    
	<xsl:template match="rater">

		  <tr> <td> <xsl:text>Reviewer name:</xsl:text> </td> <td> <xsl:text> Date </xsl:text> </td> </tr> 
		  <tr> <td> <xsl:value-of select="@user"/> </td> <td> <xsl:value-of select="@date"/> </td> </tr>

		  <xsl:apply-templates select="*">
	      	<xsl:sort select="@name" />
	      </xsl:apply-templates> 

	</xsl:template>
		
	<xsl:template match="rating">
		  <tr> <td> <xsl:text> Rating types used: </xsl:text> </td> <td> </td> </tr> 
		  <tr> <td> <xsl:value-of select="@name"/> </td> <td> </td> </tr>
	</xsl:template>	

</xsl:stylesheet>
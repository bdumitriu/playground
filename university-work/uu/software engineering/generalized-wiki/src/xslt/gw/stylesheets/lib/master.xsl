<?xml version="1.0"?>

<!-- Changed to new GWML -->

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0"
	  xmlns:repep="gw.render.extensions.ExtensionFunctions"
	  >

  	<xsl:template name="css_header">
  		<link rel="stylesheet" type="text/css" media="screen" href="/gw/styles/style.css" />
  		<link rel="stylesheet" type="text/css" media="screen" href="/gw/styles/layout.css" />
  		<link rel="stylesheet" type="text/css" media="screen" href="/gw/styles/gwml.css" />
  	</xsl:template>

	
	<!-- Head titles -->
	<xsl:template match="head">
		<h1 class="twikiTopicTitle"><xsl:value-of select="@title" /></h1>
		<div class="twikiWebTitle"><xsl:value-of select="@subtitle" /></div>
	</xsl:template>
	
	<!--Table of contents -->
	<xsl:template match="toc">
		<div class="gwToc">
  			<xsl:for-each select="//title">
  				<xsl:variable name="temp"><xsl:apply-templates select="ancestor::*" mode="depthcounter"/></xsl:variable>
				<xsl:variable name="depth"><xsl:value-of select="string-length($temp)-1"/></xsl:variable>
				<div class="depth{$depth}">
					<a href="#{generate-id(.)}">
						<xsl:value-of select="."/>
					</a>
				</div>
			</xsl:for-each>
		</div>
	</xsl:template>
	
	<!-- Section -->
	<xsl:template match="section">
		<div class="section">
			<!--Optional title -->
			<xsl:if test="title">
				<xsl:apply-templates select="title"/>
			</xsl:if>
			<xsl:if test="include">
				<xsl:apply-templates select="include"/>
			</xsl:if>

			<xsl:apply-templates select="para | section"/>
		</div>
	</xsl:template>		
	
	<!-- title -->
	<xsl:template match="title">
		<!-- Calculate section node depth -->
		<xsl:variable name="temp"><xsl:apply-templates select="ancestor::*" mode="depthcounter"/></xsl:variable>
		<xsl:variable name="depth"><xsl:value-of select="string-length($temp)-1"/></xsl:variable>
		<div class="title">
			<div class="h{$depth}">
				<a name="{generate-id(.)}">
					<xsl:value-of select="."/>
				</a>
			</div>
		</div>
	</xsl:template>

	<!-- Calculate section node depth -->
	<xsl:template match = "*" mode = "depthcounter" > 
		<xsl:value-of select = "position()" /> 
	</xsl:template> 		
	
	<!-- Paragraph -->
	<xsl:template match="para">
		<div class="para">
			<xsl:apply-templates />
		</div>
	</xsl:template>

	<!-- Link -->
	<xsl:template match="link">
		<a href="{@target}">
         <xsl:variable name="text"><xsl:value-of select="@text"/></xsl:variable>
			<xsl:variable name="linktext"><xsl:value-of select="." /></xsl:variable>

         <xsl:choose>
                 <xsl:when test="string-length($linktext) > 0">
                         <xsl:value-of select="$linktext" />
                 </xsl:when>
                 <xsl:when test="string-length($text) > 0">
                         <xsl:value-of select="$text" />
                 </xsl:when>
                 <xsl:otherwise>
                         <xsl:value-of select="@target" />
                 </xsl:otherwise>
         </xsl:choose>
		</a>
  </xsl:template>	

	<!-- Lists -->
	<xsl:template match="list">
		<xsl:choose>
			<xsl:when test="string-length(@type) = 0">
				<!-- Unordered lists -->
				<ul>
					<xsl:apply-templates select="item | list" />
				</ul>
			</xsl:when>
			<xsl:otherwise>
				<!-- Ordered lists -->
				<ol type="{@type}">
					<xsl:apply-templates select="item | list" />
				</ol>
			</xsl:otherwise>
		</xsl:choose>
	</xsl:template>
	
	<!-- List Item -->
	<xsl:template match="item">
		<li><xsl:apply-templates /></li>
	</xsl:template>
	
	<!-- Bold -->
	<xsl:template match="b">
		<b><xsl:apply-templates /></b>
	</xsl:template>	

	<!-- Italic -->
	<xsl:template match="i">
		<i><xsl:apply-templates /></i>
	</xsl:template>	

	<!-- Breakline -->
	<xsl:template match="br">
		<br />
	</xsl:template>
	
	<!-- Pre -->
	<xsl:template match="pre">
		<pre><xsl:value-of select="." /></pre>
	</xsl:template>	

	<!-- Code -->
	<xsl:template match="code">
		<code>
		<xsl:variable name="gwmlcontent"><xsl:value-of select="."/></xsl:variable>
		<xsl:value-of select="repep:escapeTags($gwmlcontent)"/>
		</code>
	</xsl:template>	
	
	<!-- Tt -->
	<xsl:template match="tt">
		<tt><xsl:value-of select="."/></tt>
	</xsl:template>			
	
	<!-- Image -->
	<xsl:template match="img">
		<img src="{@target}" width="{@width}" height="{@height}" />
	</xsl:template>	
	
	<!-- Horizontal rule -->
	<xsl:template match="hr">
		<hr />
	</xsl:template>
	
	<!-- Tables -->
	<xsl:template match="table">
		<table widht="{@width}">
			<xsl:apply-templates select="row | col" />
		</table>
	</xsl:template>	

	<!-- Row -->
	<xsl:template match="row">
		<xsl:choose>
			<xsl:when test="col">
				<tr><xsl:apply-templates select="col" /></tr>
			</xsl:when>
			<xsl:otherwise>
				<tr><td><xsl:value-of select="." /></td></tr>
			</xsl:otherwise>
		</xsl:choose>
	</xsl:template>
	
	<!-- Col -->
	<xsl:template match="col">
		<xsl:choose>
			<!-- FIXME: if table as parent, then only one tr tage -->
			<xsl:when test="parent::table">
				<td width="{@width}"><xsl:apply-templates /></td>
			</xsl:when>
			<xsl:otherwise>
				<td width="{@width}"><xsl:apply-templates /></td>
			</xsl:otherwise>
		</xsl:choose>
	</xsl:template>
	
	<!-- Forms -->
	<xsl:template match="form">
		<form method="{@method}" action="{@action}">
			<xsl:apply-templates />
		</form>
	</xsl:template>
	
	<!-- Input -->
	<xsl:template match="input">
		<input type="{@type}" name="{@name}" maxlength="{@maxlength}" />
	</xsl:template>
	
	<!-- Textarea -->
	<xsl:template match="textarea">
		<textarea name="{@name}"><xsl:value-of select="." /></textarea>
	</xsl:template>	
	
	<!--wildInclude-->
    <xsl:template name="wildinclude">
    	<xsl:param name="content" select="repep:wildInclude(@target)"/>
    	<!--Change value into copy to get the nodes, see html-source-->
       	<xsl:value-of select="$content/document/*"/>
	</xsl:template>
  	
  	<!--rawInclude-->
    <xsl:template name="rawinclude">
		<pre>    
       		<xsl:value-of select="repep:rawInclude(@target)"/>
       	</pre>
	</xsl:template>
	
	<!--simpleinclude-->
	<xsl:template name="simpleinclude">
	    <xsl:param name="content" select="document(concat('file:///', @target))"/>
	    <!--Change value into copy to get the nodes, see html-source-->
		<xsl:value-of select="$content/document/*"/>
	</xsl:template>

	<!--include-->
    <xsl:template match="include">
		<xsl:choose>
		  <xsl:when test="@type = 'raw'">
			<xsl:call-template name="rawinclude"/>
		  </xsl:when>
		  <xsl:when test="@type = 'simple'">
		  	<xsl:call-template name="simpleinclude"/>
		  </xsl:when>
		  <xsl:otherwise>
			<xsl:call-template name="wildinclude"/>
		  </xsl:otherwise>
		</xsl:choose>
	</xsl:template>
	
</xsl:stylesheet>


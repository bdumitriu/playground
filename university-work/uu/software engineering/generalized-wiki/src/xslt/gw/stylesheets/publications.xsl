<?xml version="1.0"?>
<!DOCTYPE stylesheet
 [
  <!ENTITY nbsp "&#160;">
 ]
>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
                              xmlns:at="http://aterm.org">
  <xsl:output method="html" indent="yes"/>
  <xsl:strip-space elements="*"/>
  <xsl:include href="bibtex-fields.xsl"/>
  <xsl:template match="/Entries">
  <html>
    <head>
      <title>BibTex stylesheet</title>
    </head>
    <body>
        <dl>
        <xsl:for-each select="at:list/at:tuple/Entry" >

           <xsl:sort select="at:list/Field[at:string = 'year']/." order="descending"/>
           <xsl:sort select="at:list/Field[at:string = 'author']/Words/at:list/at:string" /> 
		



           <xsl:variable name="path">
               <xsl:choose>
                   <xsl:when test="at:list/Field/Id/at:string">
                       <xsl:value-of select="at:list/Field/Id/at:string" />
                    </xsl:when>
                    <xsl:otherwise>
                        <xsl:value-of select="at:list/Field[at:string = 'year']/*[name()='Qwords' or name()= 'Words']/at:list/at:string" />
                  	</xsl:otherwise>
                </xsl:choose>
            </xsl:variable>
            <xsl:choose>
                <xsl:when test="position() = 1">
                    <h4 style="background-color:#90AFFF"><xsl:value-of select="$path" /></h4>
                </xsl:when>
                <xsl:when test="position() > 1">
                    <xsl:variable name="tmp">
                        <xsl:for-each select="../preceding-sibling::at:tuple/Entry/at:list/Field[at:string = 'year']/.">
                                <xsl:value-of select="concat(string(.),'-')" />                                
                        </xsl:for-each>
                    </xsl:variable>
                    <xsl:value-of select="$tmp"/>
                    <xsl:if test="not(contains($tmp,$path))">
                        <h4 style="background-color:#90AFFF"><xsl:value-of select="$path"/></h4>
                    </xsl:if>
                </xsl:when>
            </xsl:choose>
            
            <dt>[<xsl:value-of select="position()" />]</dt>       
            <dd>
             <xsl:choose>
                <xsl:when test="translate(at:string,$upper-case,$lower-case) = 'inproceedings'">
                   <xsl:call-template name="inproceedings"/>
                </xsl:when>
                <xsl:when test="translate(at:string,$upper-case,$lower-case) = 'article'">
                   <xsl:call-template name="article"/>
                </xsl:when>
                <xsl:when test="translate(at:string,$upper-case,$lower-case) = 'techreport'">
                   <xsl:call-template name="techreport"/>
                </xsl:when>
                <xsl:when test="translate(at:string,$upper-case,$lower-case) = 'incollection'">
                   <xsl:call-template name="incollection"/>
                </xsl:when>
                <xsl:when test="translate(at:string,$upper-case,$lower-case) = 'phdthesis'">
                   <xsl:call-template name="phdthesis"/>
                </xsl:when>
                <xsl:when test="translate(at:string,$upper-case,$lower-case) = 'mastersthesis'">
                   <xsl:call-template name="mastersthesis"/>
                </xsl:when>
                <xsl:when test="translate(at:string,$upper-case,$lower-case) = 'misc'">
                   <xsl:call-template name="misc"/>
                </xsl:when>
                <xsl:when test="translate(at:string,$upper-case,$lower-case) = 'unpublished'">
                   <xsl:call-template name="unpublished"/>
                </xsl:when>
                <xsl:when test="translate(at:string,$upper-case,$lower-case) = 'manual'">
                   <xsl:call-template name="manual"/>
                </xsl:when>
             </xsl:choose>

            </dd>
            <br/>
        </xsl:for-each>
        </dl>
   </body>
 </html>
</xsl:template>

<xsl:template name="inproceedings">
<!-- author -->
<xsl:call-template name="author"><xsl:with-param name="pname" select="'author'" /></xsl:call-template>
<!-- title -->
<xsl:call-template name="title"/>
<!-- editor -->
<xsl:call-template name="author"><xsl:with-param name="pname" select="'editor'" /></xsl:call-template>,
<!-- booktitle -->
<i><xsl:call-template name="generic_entry"><xsl:with-param name="entry" select="'booktitle'" /></xsl:call-template></i>,
<!-- volume -->
<xsl:call-template name="volume"/>
<!-- series  -->
<i><xsl:call-template name="generic_entry"><xsl:with-param name="entry" select="'series'" /></xsl:call-template></i>,
<!-- number -->
<xsl:call-template name="number"><xsl:with-param name="entry" select="'inproceedings'" /></xsl:call-template>
<!-- pages -->
<xsl:call-template name="pages"/>,
<!-- address  -->
<xsl:call-template name="generic_entry"><xsl:with-param name="entry" select="'address'" /></xsl:call-template>,
<!-- month -->
<xsl:call-template name="date"><xsl:with-param name="part" select="'month'" /></xsl:call-template>
<!-- year -->
<xsl:call-template name="date"><xsl:with-param name="part" select="'year'" /></xsl:call-template>.
<!-- publisher -->
<xsl:call-template name="generic_entry"><xsl:with-param name="entry" select="'publisher'" /></xsl:call-template>.
<!-- links -->
<xsl:call-template name="link"/>
</xsl:template>

<xsl:template name="article">
<!-- author -->
<xsl:call-template name="author"><xsl:with-param name="pname" select="'author'" /></xsl:call-template>
<!-- title -->
<xsl:call-template name="title"/>
<!-- journal -->
<i><xsl:call-template name="generic_entry"><xsl:with-param name="entry" select="'journal'" /></xsl:call-template></i>,
<!-- volume -->
<xsl:call-template name="volume"><xsl:with-param name="entry" select="'article'" /></xsl:call-template>
<!-- number -->
<xsl:call-template name="number"><xsl:with-param name="entry" select="'article'" /></xsl:call-template>
<!-- pages -->
<!--xsl:call-template name="pages"><xsl:with-param name="entry" select="'article'" /></xsl:call-template-->
<!-- month -->
<xsl:call-template name="date"><xsl:with-param name="part" select="'month'" /></xsl:call-template>
<!-- year -->
<xsl:call-template name="date"><xsl:with-param name="part" select="'year'" /></xsl:call-template>.
<!-- note -->
<xsl:call-template name="generic_entry"><xsl:with-param name="entry" select="'note'" /></xsl:call-template>
<!-- links -->
<xsl:call-template name="link"/>
</xsl:template>

<xsl:template name="techreport">
<!-- author -->
<xsl:call-template name="author"><xsl:with-param name="pname" select="'author'" /></xsl:call-template>
<!-- title -->
<xsl:call-template name="title"/>
<!-- pubcat -->
Technical Report <!--xsl:call-template name="generic_entry"><xsl:with-param name="entry" select="'pubcat'" /></xsl:call-template-->
<!-- number -->
<xsl:call-template name="number"/>
<!-- institution -->
<xsl:call-template name="generic_entry"><xsl:with-param name="entry" select="'institution'" /></xsl:call-template>
<!-- address  -->
<xsl:call-template name="generic_entry"><xsl:with-param name="entry" select="'address'" /></xsl:call-template>,
<!-- month -->
<xsl:call-template name="date"><xsl:with-param name="part" select="'month'" /></xsl:call-template>
<!-- year -->
<xsl:call-template name="date"><xsl:with-param name="part" select="'year'" /></xsl:call-template>.
<!-- note -->
<xsl:call-template name="generic_entry"><xsl:with-param name="entry" select="'note'" /></xsl:call-template>
<!-- links -->
<xsl:call-template name="link"/>
</xsl:template>

<xsl:template name="incollection">
<!-- author -->
<xsl:call-template name="author"><xsl:with-param name="pname" select="'author'" /></xsl:call-template>
<!-- title -->
<xsl:call-template name="title"/>
<!-- editor -->
<xsl:call-template name="author"><xsl:with-param name="pname" select="'editor'" /></xsl:call-template>,
<!-- booktitle -->
<i><xsl:call-template name="generic_entry"><xsl:with-param name="entry" select="'booktitle'" /></xsl:call-template></i>,
<!-- volume -->
volume <xsl:call-template name="generic_entry"><xsl:with-param name="entry" select="'volume'" /></xsl:call-template>
<!-- series -->
of<i><xsl:call-template name="generic_entry"><xsl:with-param name="entry" select="'series'" /></xsl:call-template></i>,
<!-- pages -->
<xsl:call-template name="pages"/>.
<!-- publisher -->
<xsl:call-template name="generic_entry"><xsl:with-param name="entry" select="'publisher'" /></xsl:call-template>,
<!-- address  -->
<xsl:call-template name="generic_entry"><xsl:with-param name="entry" select="'address'" /></xsl:call-template>
<!-- month -->
<!--xsl:call-template name="date"><xsl:with-param name="part" select="'month'" /></xsl:call-template-->
<!-- year -->
<xsl:call-template name="date"/>.<!--xsl:with-param name="part" select="'year'" /></xsl:call-template-->.
<!-- links -->
<xsl:call-template name="link"/>
</xsl:template>

<xsl:template name="phdthesis">
<!-- author -->
<xsl:call-template name="author"><xsl:with-param name="pname" select="'author'" /></xsl:call-template>
<!-- title -->
<xsl:call-template name="title"/>
PhD thesis,
<!-- school -->
<xsl:call-template name="generic_entry"><xsl:with-param name="entry" select="'school'" /></xsl:call-template>,
<!-- address  -->
<xsl:call-template name="generic_entry"><xsl:with-param name="entry" select="'address'" /></xsl:call-template>
<!-- month -->
<!-- <xsl:call-template name="date"><xsl:with-param name="part" select="'month'" /></xsl:call-template> -->
<!-- year -->
<xsl:call-template name="date"/>.
<!-- note -->
<xsl:call-template name="generic_entry"><xsl:with-param name="entry" select="'note'" /></xsl:call-template>
<!-- links -->
<xsl:call-template name="link"/>
</xsl:template>

<xsl:template name="mastersthesis">
<!-- author -->
<xsl:call-template name="author"><xsl:with-param name="pname" select="'author'" /></xsl:call-template>
<!-- title -->
<xsl:call-template name="title"/>
Master&apos;s thesis,
<!-- school -->
<xsl:call-template name="generic_entry"><xsl:with-param name="entry" select="'school'" /></xsl:call-template>,
<!-- address  -->
<xsl:call-template name="generic_entry"><xsl:with-param name="entry" select="'address'" /></xsl:call-template>
<!-- date -->
<xsl:call-template name="date"/>.
<!-- note -->
<xsl:call-template name="generic_entry"><xsl:with-param name="entry" select="'note'" /></xsl:call-template>
<!-- links -->
<xsl:call-template name="link"/>
</xsl:template>

<xsl:template name="misc">
<!-- author -->
<xsl:call-template name="author"><xsl:with-param name="pname" select="'author'" /></xsl:call-template>
<!-- title -->
<xsl:call-template name="title"/>
<!-- date -->
<xsl:call-template name="date"/>.
<!-- note -->
<xsl:call-template name="generic_entry"><xsl:with-param name="entry" select="'note'" /></xsl:call-template>
<!-- links -->
<xsl:call-template name="link"/>
</xsl:template>

<xsl:template name="unpublished">
<!-- author -->
<xsl:call-template name="author"><xsl:with-param name="pname" select="'author'" /></xsl:call-template>
<!-- title -->
<xsl:call-template name="title"/>
Master&apos;s thesis,
<!-- note -->
<xsl:call-template name="generic_entry"><xsl:with-param name="entry" select="'note'" /></xsl:call-template>,
<!-- date -->
<xsl:call-template name="date"/>.
<!-- links -->
<xsl:call-template name="link"/>
</xsl:template>

<xsl:template name="manual">
<!-- author -->
<xsl:call-template name="author"><xsl:with-param name="pname" select="'author'" /></xsl:call-template>
<!-- title -->
<xsl:call-template name="title"/>
<!-- organization -->
<xsl:call-template name="generic_entry"><xsl:with-param name="entry" select="'organization'" /></xsl:call-template>,
<!-- address  -->
<xsl:call-template name="generic_entry"><xsl:with-param name="entry" select="'address'" /></xsl:call-template>
<!-- date -->
<xsl:call-template name="date"/>.
<!-- note -->
<xsl:call-template name="generic_entry"><xsl:with-param name="entry" select="'note'" /></xsl:call-template>,
<!-- links -->
<xsl:call-template name="link"/>
</xsl:template>

</xsl:stylesheet>

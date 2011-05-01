<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                              xmlns:xalan="http://xml.apache.org/xalan"
			      xmlns:bibtex="gw.util.BibTex"
			      xmlns:bib="gw.render.extensions.ExtensionFunctions"
                              extension-element-prefixes="bibtex bib xalan">

<xsl:include href="resource:/gw/stylesheets/lib/bibtex-fields.xsl"/>
<xsl:import href="resource:/gw/stylesheets/lib/master.xsl" />

<xsl:template name="viewEntries">
	<!-- take all the nodes on which has to be sorted and put them in seperate sortby tags in a nodelist -->
        <xsl:variable name="nodelist">
            <xsl:if test="$sortField1 != ''">
            <xsl:for-each select="list/tuple/Entry" >
               <xsl:sort select="list/Field[string = $sortField1]/*[name()='Qwords' or name()= 'Words']/list/string" order="descending"/>
               <sortby>
                  <xsl:value-of select="list/Field[string = $sortField1]/*[name()='Qwords' or name()= 'Words']/list/string"/>
                </sortby>
           </xsl:for-each>
           </xsl:if>
        </xsl:variable>

        <!-- For each Entry process the sort value and put it in the path variable -->
        <xsl:for-each select="list/tuple/Entry" >
	   <xsl:sort select="bibtex:processSortunder(list/Field[string =$sortField1]/*[name()='Qwords' or name()= 'Words']/list/string)" order="descending" />

           <xsl:variable name="path">
               <xsl:choose>
                   <xsl:when test="list/Field[string = $sortField1]/Id/string">
                       <xsl:value-of select="list/Field[string = $sortField1]/Id/string" />
                    </xsl:when>
                    <xsl:otherwise>
                        <xsl:value-of select="list/Field[string = $sortField1]/*[name()='Qwords' or name()= 'Words']/list/string" />
                  	</xsl:otherwise>
                </xsl:choose>
            </xsl:variable>

            <dl>
                <xsl:if test="$sortField1 != ''">
                <xsl:choose>
                    <xsl:when test="position() = 1 or not(../preceding-sibling::*)">
                        <h4 style="background-color:#90AFFF"><xsl:value-of select="$path" /></h4>
                    </xsl:when>
                    <xsl:otherwise>
                        <xsl:variable name="curr" select="position()" />
                        <xsl:variable name="cj" select="$path" />
                        <xsl:variable name="print">
                            <xsl:for-each select="xalan:nodeset($nodelist)/sortby[position() &lt; $curr]">
                                <xsl:if test=" . != $cj">
                                    <xsl:value-of select="'no'" />
                                </xsl:if>
                                <xsl:if test=" . = $cj">
                                    <xsl:value-of select="'yes'" />
                                </xsl:if>
                            </xsl:for-each>
                        </xsl:variable>
                        <xsl:if test="not(contains($print,'yes'))">
                           <h4 style="background-color:#90AFFF"><xsl:value-of select="$cj" /></h4>
                        </xsl:if>
                    </xsl:otherwise>
                </xsl:choose>
                </xsl:if>

                <dt>[<xsl:value-of select="position()" />]</dt>       
                <dd>
                 <xsl:choose>
                    <xsl:when test="translate(string,$upper-case,$lower-case) = 'inproceedings'">
                       <xsl:call-template name="inproceedings"/>
                    </xsl:when>
                    <xsl:when test="translate(string,$upper-case,$lower-case) = 'article'">
                       <xsl:call-template name="article"/>
                    </xsl:when>
                    <xsl:when test="translate(string,$upper-case,$lower-case) = 'techreport'">
                       <xsl:call-template name="techreport"/>
                    </xsl:when>
                    <xsl:when test="translate(string,$upper-case,$lower-case) = 'incollection'">
                       <xsl:call-template name="incollection"/>
                    </xsl:when>
                    <xsl:when test="translate(string,$upper-case,$lower-case) = 'phdthesis'">
                       <xsl:call-template name="phdthesis"/>
                    </xsl:when>
                    <xsl:when test="translate(string,$upper-case,$lower-case) = 'mastersthesis'">
                       <xsl:call-template name="mastersthesis"/>
                    </xsl:when>
                    <xsl:when test="translate(string,$upper-case,$lower-case) = 'misc'">
                       <xsl:call-template name="misc"/>
                    </xsl:when>
                    <xsl:when test="translate(string,$upper-case,$lower-case) = 'unpublished'">
                       <xsl:call-template name="unpublished"/>
                    </xsl:when>
                    <xsl:when test="translate(string,$upper-case,$lower-case) = 'manual'">
                       <xsl:call-template name="manual"/>
                    </xsl:when>
		    <xsl:when test="translate(string,$upper-case,$lower-case) = 'booklet'">
                       <xsl:call-template name="booklet"/>
                    </xsl:when>
		    <xsl:when test="translate(string,$upper-case,$lower-case) = 'book'">
                       <xsl:call-template name="book"/>
                    </xsl:when>
                 </xsl:choose>
                </dd>
                <br/>
            </dl>
        </xsl:for-each>
</xsl:template>

<xsl:template name="inproceedings">
	<!-- author -->
	<xsl:call-template name="author"><xsl:with-param name="pname" select="'author'" /></xsl:call-template>
	<!-- title -->
	<xsl:call-template name="title"/>.
	<!-- editor -->
	<xsl:call-template name="author"><xsl:with-param name="pname" select="'editor'" /></xsl:call-template>,
	<!-- booktitle -->
	<i><xsl:call-template name="generic_entry"><xsl:with-param name="entry" select="'booktitle'" /></xsl:call-template></i>,
	<!-- volume -->
	<xsl:call-template name="volume"/>
	<!-- series  -->
	<em><xsl:call-template name="generic_entry"><xsl:with-param name="entry" select="'series'" /></xsl:call-template></em>,
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
	<xsl:call-template name="title"/>.
	<!-- journal -->
	<i><xsl:call-template name="generic_entry"><xsl:with-param name="entry" select="'journal'" /></xsl:call-template></i>,
	<!-- volume -->
	<xsl:call-template name="volume"><xsl:with-param name="entry" select="'article'" /></xsl:call-template>
	<!-- number -->
	<xsl:call-template name="number"><xsl:with-param name="entry" select="'article'" /></xsl:call-template>
	<!-- pages -->
	<xsl:call-template name="pages"><xsl:with-param name="entry" select="'article'" /></xsl:call-template>,
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
	<xsl:call-template name="title"/>.
	<!-- pubcat -->
	Technical Report <xsl:call-template name="generic_entry"><xsl:with-param name="entry" select="'pubcat'" /></xsl:call-template>
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
	<xsl:call-template name="title"/>.
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
	<!-- date -->
	<xsl:call-template name="date"/>.
	<!-- links -->
	<xsl:call-template name="link"/>
</xsl:template>

<xsl:template name="phdthesis">
	<!-- author -->
	<xsl:call-template name="author"><xsl:with-param name="pname" select="'author'" /></xsl:call-template>
	<!-- title -->
	<xsl:call-template name="title"/>.
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
	.Master&apos;s thesis,
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
	<xsl:call-template name="title"/>.
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
	<xsl:call-template name="title"/>.
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
	<xsl:call-template name="title"/>.
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

<xsl:template name="inbook">
	<!-- author -->
	<xsl:call-template name="author"><xsl:with-param name="pname" select="'author'" /></xsl:call-template>
	<!-- title -->
	<em><xsl:call-template name="title"><xsl:with-param name="entry" select="'inbook'" /></xsl:call-template></em>,
	<!-- volume -->
	<xsl:call-template name="volume"/>
	<!-- series  -->
	<em><xsl:call-template name="generic_entry"><xsl:with-param name="entry" select="'series'" /></xsl:call-template></em>,
	<!-- chapter -->
	<xsl:call-template name="chapter"><xsl:with-param name="entry" select="'inbook'" /></xsl:call-template>
	<xsl:call-template name="pages"/>.
	<!-- publisher -->
	<xsl:call-template name="generic_entry"><xsl:with-param name="entry" select="'publisher'" /></xsl:call-template>,
	<!-- address -->
	<xsl:call-template name="generic_entry"><xsl:with-param name="entry" select="'address'" /></xsl:call-template>,
	<!-- edition -->
	<xsl:call-template name="generic_entry"><xsl:with-param name="entry" select="'edition'" /></xsl:call-template>,
	<!-- date -->
	<xsl:call-template name="date"/>.
	<!-- links -->
	<xsl:call-template name="link"/>
</xsl:template>

<xsl:template name="book">
	<!-- author -->
	<xsl:call-template name="author"><xsl:with-param name="pname" select="'author'" /></xsl:call-template>
	<!-- title -->
	<em><xsl:call-template name="title"/></em>.
	<!-- volume -->
	<xsl:call-template name="volume"/>
	<!-- series  -->
	<em><xsl:call-template name="generic_entry"><xsl:with-param name="entry" select="'series'" /></xsl:call-template></em>,
	<!-- publisher -->
	<xsl:call-template name="generic_entry"><xsl:with-param name="entry" select="'publisher'" /></xsl:call-template>,
	<!-- address -->
	<xsl:call-template name="generic_entry"><xsl:with-param name="entry" select="'address'" /></xsl:call-template>,
	<!-- edition -->
	<xsl:call-template name="generic_entry"><xsl:with-param name="entry" select="'edition'" /></xsl:call-template>,
	<!-- date -->
	<xsl:call-template name="date"/>.
	<!-- note -->
	<xsl:call-template name="generic_entry"><xsl:with-param name="entry" select="'note'" /></xsl:call-template>.
	<!-- links -->
	<xsl:call-template name="link"/>
</xsl:template> 

<xsl:template name="booklet">
	<!-- author -->
	<xsl:call-template name="author"><xsl:with-param name="pname" select="'author'" /></xsl:call-template>.
	<!-- title -->
	<xsl:call-template name="title"/>
	<!-- address -->
	<xsl:call-template name="generic_entry"><xsl:with-param name="entry" select="'address'" /></xsl:call-template>
	<!-- month -->
	<xsl:call-template name="date"><xsl:with-param name="part" select="'month'" /></xsl:call-template>
	<!-- year -->
	<xsl:call-template name="date"><xsl:with-param name="part" select="'year'" /></xsl:call-template>.
	<!-- note -->
	<xsl:call-template name="generic_entry"><xsl:with-param name="entry" select="'note'" /></xsl:call-template>.
</xsl:template> 

</xsl:stylesheet>

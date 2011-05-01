<?xml version="1.0"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<!-- declarations -->
<xsl:variable name="upper-case" select="'ABCDEFGHIJKLMNOPQRSTUVWXYZ'"/>
<xsl:variable name="lower-case">abcdefghijklmnopqrstuvwxyz</xsl:variable>
<!-- Author -->
<xsl:template name="author">
   <xsl:param name="pname" />
    <xsl:if test="$pname = 'editor'">In </xsl:if>
   <!-- no comma's in author entry -->
   <xsl:if test="not(contains(list/Field[string = $pname]/Words/list/., ','))" >
     <!-- first initial -->
     <xsl:value-of select="substring(translate(list/Field[string = $pname]/Words/list/string[1],'{\',''),1,1)"/>.

     <xsl:for-each select="list/Field[string = $pname]/Words/list/string[position() > 1]">
     <xsl:choose>
       <!-- don't parse 'others' -->
       <xsl:when test=". = 'others'"/>
       <!-- firstname after 'and' -->
       <xsl:when test="preceding-sibling::string[1] = 'and' ">
         <xsl:value-of select="concat(' ',substring(.,1,1),'.')"/>
       </xsl:when>
       <!-- middlename -->     
       <xsl:when test="position() != last() and following-sibling::string[1] != 'and' and text()!='and'">
         <!-- if the name start with an uppercase latter then it's a middlename -->
         <xsl:if test="contains($upper-case,substring(.,1,1))">
            <xsl:value-of select="concat(' ',substring(.,1,1),'.')"/>
         </xsl:if>
         <!-- if not then it's voorzetsel -->
         <xsl:if test="not(contains($upper-case,substring(.,1,1)))">
            <xsl:value-of select="concat(' ',.)"/>
         </xsl:if>
       </xsl:when>
       <!-- lastname is not the last one in the entry  -->
       <xsl:when test="position() != last() and text() != 'and'">
         <xsl:value-of select="concat(' ',.)"/>
       </xsl:when>
       <!-- match 'and others' -->
       <xsl:when test=" text() = 'and'">
         <xsl:variable name="rest" >
            <xsl:for-each select="following-sibling::string">
               <xsl:value-of select="."/>
            </xsl:for-each>
         </xsl:variable>
         <xsl:choose>
             <xsl:when test="contains($rest,'others')">
               <xsl:choose>
                   <xsl:when test="$pname = 'editor'">
                      <xsl:text> et al.,</xsl:text>
                   </xsl:when>
                   <xsl:otherwise>
                      <xsl:text> et al.</xsl:text>
               	   </xsl:otherwise>
               </xsl:choose>
             </xsl:when>
             <xsl:when test="contains($rest,'and')">
                 <xsl:text>, </xsl:text>
             </xsl:when>
             <xsl:otherwise>
                 <xsl:text> and </xsl:text>
         	</xsl:otherwise>
         </xsl:choose>
       </xsl:when>
       <!-- last lastname in the enrty  -->
       <xsl:otherwise>
          <xsl:choose>
             <xsl:when test="$pname = 'editor'">
                <xsl:value-of select="concat(' ',.)"/>,
             </xsl:when>
             <xsl:otherwise>
                <xsl:value-of select="concat(' ',.)"/>.
             </xsl:otherwise>
          </xsl:choose>
       </xsl:otherwise>
     </xsl:choose>
    </xsl:for-each>
    <xsl:choose>
       <xsl:when test="$pname = 'editor' and count(list/Field[string = $pname]/Words/list/*[.='and']) = 0">
          <xsl:text> editor</xsl:text>
       </xsl:when>
       <xsl:when test="$pname = 'editor'">
          <xsl:text> editors</xsl:text>
   	   </xsl:when>
    </xsl:choose>
   </xsl:if>
</xsl:template>

<xsl:template name="date">
    <!-- date -->
    <xsl:param name="part"></xsl:param>
    <xsl:choose>
        <xsl:when test="$part != ''">
            <xsl:call-template name="separate_date"><xsl:with-param name="part" select="$part" /></xsl:call-template>
        </xsl:when>
        <xsl:otherwise>
            <xsl:choose>
               <xsl:when test="list/Field[string = 'month' or string = 'year']/Id/string">
                  <xsl:value-of select="list/Field[string = 'month']/Id/string"/>
                  <xsl:value-of select="list/Field[string = 'year']/Id/string"/>
               </xsl:when>
               <xsl:otherwise>
               <xsl:if test="list/Field[string = 'month']/*[name() = 'QWords' or name() = 'Words']/list/string">
                  <xsl:for-each select="list/Field[string = 'month']/*[name() = 'QWords' or name() = 'Words']/list/string">
                    <xsl:value-of select="concat(.,' ')"/>
                  </xsl:for-each>
               </xsl:if>
               <xsl:if test="list/Field[string = 'year']/*[name() = 'QWords' or name() = 'Words']/list/string">
                  <xsl:for-each select="list/Field[string = 'year']/*[name() = 'QWords' or name() = 'Words']/list/string">
                    <xsl:value-of select="concat(.,' ')"/>
                  </xsl:for-each>
               </xsl:if>
               </xsl:otherwise>
            </xsl:choose>
    	</xsl:otherwise>
    </xsl:choose>
</xsl:template>

<xsl:template name="separate_date">
    <!-- date -->
    <xsl:param name="part"></xsl:param>
    <xsl:choose>
       <xsl:when test="list/Field[string = $part]/Id/string">
          <xsl:value-of select="list/Field[string = $part]/Id/string"/>
       </xsl:when>
       <xsl:otherwise>
          <xsl:for-each select="list/Field[string = $part]/*[name() = 'QWords' or name() = 'Words']/list/string">
            <xsl:value-of select="concat(.,' ')"/>            
          </xsl:for-each>
       </xsl:otherwise>
    </xsl:choose>
</xsl:template>

<xsl:template name="pages">
   <xsl:param name="entry"></xsl:param>
    <!-- date -->
   <!--xsl:value-of select="list/Field[string = 'pages']/*[name() = 'QWords' or name='Words']/list/string"/>,-->
   <!--xsl:choose>
       <xsl:when test="$entry = 'article'">:</xsl:when>
       <xsl:otherwise>*pages* </xsl:otherwise>
   </xsl:choose-->
   <xsl:variable name="printpages">
       <xsl:for-each select="list/Field[string = 'pages']/*[name() = 'QWords' or name() = 'Words']/list/string">
            <xsl:choose>
                <xsl:when test="position() = last()">
                    <xsl:value-of select="." />
                </xsl:when>
                <xsl:otherwise>
                    <xsl:value-of select="concat(.,' ')" />
            	</xsl:otherwise>
            </xsl:choose>
       </xsl:for-each>
    </xsl:variable>
    <xsl:choose>
       <xsl:when test="$printpages != '' and $entry = 'article'">
          <xsl:value-of select="concat(': ',$printpages)" />
        </xsl:when>
       <xsl:when test="$printpages != ''">
          <xsl:choose>
              <xsl:when test="contains($printpages,',') or contains($printpages,'-')">
                 <xsl:value-of select="concat('pages ',$printpages)" />
              </xsl:when>
              <xsl:otherwise>
                 <xsl:value-of select="concat('page ',$printpages)" />
          	</xsl:otherwise>
          </xsl:choose>
       </xsl:when>
   </xsl:choose>
</xsl:template>

<!-- *********************** -->


<xsl:template name="title">
   <!-- title -->
   <xsl:param name="entry"/>
   <a href="{list/Field[contains(string,'url')]/Words/list/string}">
   <xsl:for-each select="list/Field[string = 'title']/Words/list/*[name() = 'string' or name() = 'Group']">
       <xsl:choose>
           <xsl:when test="name() = 'Group'">
                <xsl:for-each select="./list/string">
                    <xsl:choose>
                        <xsl:when test="position() = last()">
                            <xsl:value-of select="." />
                        </xsl:when>
                        <xsl:otherwise>
                            <xsl:value-of select="concat(.,' ')" />
                    	</xsl:otherwise>
                    </xsl:choose>
                </xsl:for-each>
           </xsl:when>
           <xsl:otherwise>
              <xsl:value-of select="translate(.,'\{}','')" />
           </xsl:otherwise>
       </xsl:choose>
       <xsl:if test="position() != last()">
          <xsl:text> </xsl:text>
       </xsl:if>
   </xsl:for-each>
   </a>
</xsl:template>

<xsl:template name="link">
      (<xsl:for-each select="list/Field[contains(string,'url') and string != 'url']">
      <a href="{Words/list/string}"><xsl:value-of select="substring-after(string,'url')"/></a>
      <xsl:if test="position() != last()">, </xsl:if> 
   </xsl:for-each>).      
</xsl:template>


<xsl:template name="number">
    <xsl:param name="entry"/>
    <xsl:if test="list/Field[string = 'number']">
      <xsl:choose>
          <xsl:when test="$entry = 'article'">
             (<xsl:call-template name="number_body">
                <xsl:with-param name="entry" select="'article'" />
              </xsl:call-template>)
          </xsl:when>
          <xsl:when test="$entry = 'inproceedings'">
            number <xsl:call-template name="number_body"/>,
          </xsl:when>
          <xsl:otherwise>
            <xsl:call-template name="number_body"/>
      	</xsl:otherwise>
      </xsl:choose>
    </xsl:if>
</xsl:template>

<xsl:template name="number_body" >
  <xsl:param name="entry" select="''"/>
  <xsl:choose>
      <xsl:when test="list/Field[string = 'number']/*[name() = 'Words' or name() = 'QWords']/list">
        <xsl:for-each select="list/Field[string = 'number']/*[name() = 'Words' or name() = 'QWords']/list/string">
        <xsl:choose>
               <xsl:when test="position() = last() and $entry = 'article'">
                 <xsl:value-of select="."/>
               </xsl:when>
                <xsl:when test="position() = last()">
                 <xsl:value-of select="."/>
               </xsl:when>
               <xsl:otherwise><xsl:value-of select="concat(.,' ')"/></xsl:otherwise>
            </xsl:choose>
        </xsl:for-each>
      </xsl:when>
      <xsl:otherwise>
         <xsl:value-of select="list/Field[string = 'number']/Id/string" />
  	</xsl:otherwise>
  </xsl:choose>
</xsl:template>



<!-- adres,publisher... -->
<xsl:template name="generic_entry">
   <xsl:param name="entry"></xsl:param>
   <xsl:text> </xsl:text>
   <xsl:if test="not(list/Field[string = 'editor']) and entry = 'booktitle'">
   <xsl:text>In</xsl:text>
   </xsl:if>
   <xsl:for-each select="list/Field[string = $entry]/*[name() = 'Words' or name() = 'QWords']/list/string">
     <xsl:choose>
        <xsl:when test="position() = last()">
           <xsl:value-of select="."/>
        </xsl:when>
        <xsl:otherwise><xsl:value-of select="concat(.,' ')"/></xsl:otherwise>
     </xsl:choose>
   </xsl:for-each>
</xsl:template>

<xsl:template name="url_command">
    <xsl:param name="field"></xsl:param>
    <xsl:choose>
        <xsl:when test="contains($field,'\url')">
            <xsl:value-of select="translate(substring-after($field,'\url'),'{}','')" />
        </xsl:when>
        <xsl:otherwise>
    	</xsl:otherwise>
    </xsl:choose>
</xsl:template>

<xsl:template name="volume">
   <xsl:param name="entry"/>
   <xsl:variable name="series" select="list/Field[string = 'series']" />
   <xsl:if test="list/Field[string = 'volume']">
       <xsl:choose>
           <xsl:when test="$entry = 'article'">
              <xsl:text> </xsl:text>
           </xsl:when>
           <xsl:otherwise>
              <xsl:text> volume</xsl:text>
       	   </xsl:otherwise>
       </xsl:choose>
      <xsl:for-each select="list/Field[string = 'volume']/*[name() = 'Words' or name() = 'QWords']/list/string">
         <xsl:choose>
            <xsl:when test="$series and position() = last()">
               <xsl:value-of select="concat(.,' of')"/>
            </xsl:when>
            <xsl:when test="not($series) and position() = last()">
               <xsl:value-of select="concat(.,',')"/>
            </xsl:when>
            <xsl:otherwise><xsl:value-of select="concat(.,' ')"/></xsl:otherwise>
         </xsl:choose>
      </xsl:for-each>
      <xsl:value-of select="list/Field[string = 'volume']/Id/string" />
   </xsl:if>
</xsl:template>

<xsl:template name="chapter">
    <xsl:param name="entry"/>
    <xsl:variable name="pages" select="list/Field[string = 'pages']" />
    <xsl:if test="list/Field[string = 'chapter']">
      <xsl:choose>
          <xsl:when test="$entry = 'inbook'">
              <xsl:choose>
                 <!-- put value of type in front of the chapter number -->
                 <xsl:when test="list/Field[string = 'type']">
                     <xsl:for-each select="list/Field[string = 'type']/*[name() = 'Words' or name() = 'QWords']/list/string">
                        <xsl:value-of select="concat(.,' ')"/>
                     </xsl:for-each>
                 </xsl:when>
                 <!-- put "chapter" in front of the chpater number -->
                 <xsl:otherwise>
                    <xsl:text>chapter</xsl:text>
              	 </xsl:otherwise>
              </xsl:choose>  
              <xsl:for-each select="list/Field[string = 'chapter']/*[name() = 'Words' or name() = 'QWords']/list/string">
                 <xsl:choose>
                    <xsl:when test="$pages and position() = last()">
                       <xsl:value-of select="concat(.,',')"/>
                    </xsl:when>
                    <xsl:when test="position() = last()">
                       <xsl:value-of select="concat(.,'.')"/>
                    </xsl:when>
                    <xsl:otherwise><xsl:value-of select="concat(.,' ')"/></xsl:otherwise>
                 </xsl:choose>
              </xsl:for-each>
          </xsl:when>
      </xsl:choose>
    </xsl:if> 
</xsl:template>

</xsl:stylesheet>
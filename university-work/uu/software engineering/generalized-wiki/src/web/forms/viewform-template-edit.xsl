<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version="1.0">

    <xsl:output method="html"/>


    <xsl:template match="/">
      <html>
        <head>
          <title>Default Form Edit Stylesheet</title>
        </head>
        <body>
          <table border="1">
            <xsl:for-each select="/root/formvalues/formentry">
              <xsl:sort select="@index"/>
              <xsl:variable name="index" select="./@index"/>
              <tr><td>
                <form method="POST">
                  <xsl:attribute name="action">
                    <xsl:choose>
                      <xsl:when test="/root/action != ''">
                        <xsl:value-of select="concat('/gw/', concat(/root/action, concat('/', /root/name)))"/>
                      </xsl:when>
                      <xsl:otherwise>
                        <xsl:value-of select="concat('/gw/form/', /root/name)"/>
                      </xsl:otherwise>
                    </xsl:choose>
                  </xsl:attribute>
                  <input type="hidden" name="_index" value="{./@index}"/>
                  
                  <table>
                    <xsl:for-each select="/root/formbase/*">
                      <tr><td>
                        <xsl:choose>
                          <xsl:when test="./@text != ''">
                            <xsl:value-of select="@text"/>
                          </xsl:when>
                          <xsl:otherwise>
                            <xsl:value-of select="name(.)"/>
                          </xsl:otherwise>
                        </xsl:choose>
                        </td><td>
                          <xsl:choose>
<!-- radio -->                            
                            <xsl:when test="@type = 'radio'">
                              <xsl:for-each select="./*">
                                <xsl:choose>
                                  <xsl:when test="@text != ''">
                                    <xsl:value-of select="@text"/>
                                  </xsl:when>
                                  <xsl:otherwise>
                                    <xsl:value-of select="name(.)"/>
                                  </xsl:otherwise>
                                </xsl:choose>
<input type="radio" name="{name(..)}" value="{name(current())}">
                                <xsl:if test="/root/formvalues/formentry[@index =
                                $index]//*[name() = name(current()/..)]/. = name(.)">
                                  <xsl:attribute name="checked"/>
                                </xsl:if>
</input><br/>
                              </xsl:for-each>
                            </xsl:when>
<!-- reset & submit -->
                            <xsl:when test="@type = 'submit' or @type = 'reset'">
<input type="{@type}" value="{current()}">
                              <xsl:if test="@type='submit'">
                                <xsl:attribute name="name">
                                  <xsl:text>submit</xsl:text>
                                </xsl:attribute>
                              </xsl:if>
</input>
                            </xsl:when>
<!-- select -->
                            <xsl:when test="@type = 'select'">
<select name="{name(.)}">
                              <xsl:if test="@size != ''">
                                <xsl:attribute name="size">
                                  <xsl:value-of select="@size"/>
                                </xsl:attribute>
                              </xsl:if>
                              <xsl:if test="@multiple != ''">
                                <xsl:attribute name="multiple"/>
                              </xsl:if>
                              <xsl:for-each select="./*">
<option value="{name(.)}">
                                <!-- single selection -->
                                <xsl:if test="/root/formvalues//*[@index =
                                $index]//*[name() = name(current()/..)]/. = name(.)">
                                  <xsl:attribute name="selected"/>
                                </xsl:if>
                                <!-- multiple selection -->
                                <xsl:if test="/root/formvalues//*[@index =
                                $index]//*[name() = name(current()/..)]//*[name() = name(current())]">
                                  <xsl:attribute name="selected"/>
                                </xsl:if>
                                <xsl:choose>
                                  <xsl:when test="@text != ''">
                                    <xsl:value-of select="@text"/>
                                  </xsl:when>
                                  <xsl:otherwise>
                                    <xsl:value-of select="name(.)"/>
                                  </xsl:otherwise>
                                </xsl:choose>
</option>
                              </xsl:for-each>
</select>
                            </xsl:when>
<!-- textarea -->
                            <xsl:when test="@type = 'textarea'">
<textarea name="{name(.)}">
                              <xsl:if test="@rows != ''">
                                <xsl:attribute name="rows">
                                  <xsl:value-of select="@rows"/>
                                </xsl:attribute>
                              </xsl:if>
                              <xsl:if test="@cols != ''">
                                <xsl:attribute name="cols">
                                  <xsl:value-of select="@cols"/>
                                </xsl:attribute>
                              </xsl:if>
                              <xsl:value-of select="/root/formvalues/formentry[@index =
                                 $index]//*[name() = name(current())]/."/>
</textarea>
                            </xsl:when>
<!-- rest:
text, password, submit, reset, hidden, file, checkbox
-->
                            <xsl:otherwise>
<input type="{@type}" name="{name(current())}">
                              <xsl:attribute name="value">
                                <xsl:value-of select="/root/formvalues/formentry[@index =
                                 $index]//*[name() = name(current())]/."/>
                              </xsl:attribute>
                              <xsl:if test="/root/formvalues/formentry[@index =
                                 $index]//*[name() = name(current())]/. =
                                 'checked'">
                                <xsl:attribute name="checked"/>
                              </xsl:if>
                              <xsl:if test="@type = ''">
                                <xsl:attribute name="type">
                                  <xsl:text>text</xsl:text>
                                </xsl:attribute>
                              </xsl:if>
                              <xsl:if test="@size != ''">
                                <xsl:attribute name="size">
                                  <xsl:value-of select="@size"/>
                                </xsl:attribute>
                              </xsl:if>
</input>
                            </xsl:otherwise>
                          </xsl:choose>
<!-- end choose -->                          
                        </td></tr>
                      </xsl:for-each>
                  </table>
                </form> 
              </td></tr>
            </xsl:for-each>
          </table>
        </body>
      </html>
    </xsl:template>

</xsl:stylesheet>                                                                                                                                                           
                                                                                                                                                            
                                                                                                                                                            
                                                                                                                                                            
                                                                                                                                                            


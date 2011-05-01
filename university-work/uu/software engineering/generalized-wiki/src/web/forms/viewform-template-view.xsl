<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version="1.0">

    <xsl:output method="html"/>


    <xsl:template match="/">
      <html>
        <head>
          <title>Default View Template</title>
        </head>
        <body>
          <table border="1">
            <xsl:for-each select="root/formvalues/formentry">
              <xsl:sort select="@index"/>
              <tr><td>
                  <table border="1">
                    <xsl:for-each select="*">
                      <tr><td>
                        <xsl:choose>
                          <xsl:when test="/root/formbase//*[name()=name(current())]/@text != ''">
                            <xsl:value-of select="/root/formbase//*[name()=name(current())]/@text"/>
                          </xsl:when>
                          <xsl:otherwise>
                            <xsl:value-of select="name(current())"/>
                          </xsl:otherwise>
                        </xsl:choose>
                        </td><td>
                        <xsl:choose>
                          <xsl:when test="/root/formbase//*[name()=name(current())]//*[name()=current()]/@text != ''">
                            <xsl:value-of select="/root/formbase//*[name()=name(current())]//*[name()=current()]/@text"/>
                          </xsl:when>
                          <xsl:otherwise>
                            <xsl:value-of select="current()"/>
                          </xsl:otherwise>
                        </xsl:choose>
                        <xsl:for-each select="*">
                        <xsl:choose>
                          <xsl:when test="/root/formbase//*[name()=name(current()/..)]//*[name()=name(current())]/@text != ''">
                            <xsl:value-of select="/root/formbase//*[name()=name(current()/..)]//*[name()=name(current())]/@text"/>
                          </xsl:when>
                          <xsl:otherwise>
                            <xsl:value-of select="name(current())"/>
                          </xsl:otherwise>
                        </xsl:choose>
<br/>
                        </xsl:for-each>
                      </td></tr>
                    </xsl:for-each>
                  </table>
              </td>
              <td>
                <form method="POST" action="{concat('/gw/form/',/root/name)}">
                  <input type="hidden" name="_index" value="{@index}"/>
                  <input type="submit" name="submit" value="Edit"/>
                  <input type="submit" name="delete" value="Delete"/>
                </form>
               </td>
              </tr>
            </xsl:for-each>
          </table>
        </body>
      </html>
    </xsl:template>

</xsl:stylesheet>                                                                                                                                                           
                                                                                                                                                            
                                                                                                                                                            
                                                                                                                                                            
                                                                                                                                                            


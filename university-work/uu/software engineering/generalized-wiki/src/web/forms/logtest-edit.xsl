<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version="1.0">

    <xsl:output method="html"/>


    <xsl:template match="/">
      <html>
        <head>
          <title>WebLog</title>
        </head>
        <body>
          <h2>WebLog test</h2>
          <table border="1">
            <xsl:for-each select="root/formvalues/formentry">
              <xsl:sort select="Date"/>
              <tr><td>
                <form action="{concat('/gw/form/', /root/name)}" method="POST">
                  <input type="hidden" name="_index" value="{./@index}"/>
                  <table><tr>
                    <xsl:for-each select="*">
                      <td>
                        <xsl:value-of select="name(current())"/>
                        <input type="text" name="{name(current())}" value="{current()}"/>
                      </td>
                    </xsl:for-each>
                    <td><input type="submit" value="Change" name="submit"/>
                    <input type="submit" value="Remove" name="delete"/>
                    </td>
                  </tr></table>
                </form> 
              </td></tr>
            </xsl:for-each>
            <tr><td>Total time:
              <xsl:value-of select="sum(/root/formvalues/formentry/Duration)"/>
            </td></tr>
          </table>
        </body>
      </html>
    </xsl:template>

</xsl:stylesheet>                                                                                                                                                           
                                                                                                                                                            
                                                                                                                                                            
                                                                                                                                                            
                                                                                                                                                            


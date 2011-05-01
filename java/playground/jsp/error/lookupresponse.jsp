<%@ page import="java.util.*, email.Map" %>

<tr>
<td width="97"> &nbsp; </td>
<td align="right"><b>Success!</b></td>
</tr>

<tr>
<td width="97"> &nbsp; </td>
<td align="right">
<jsp:getProperty name="mymap" property="name" />
<br>
<jsp:getProperty name="mymap" property="email" />
</td>
</tr>
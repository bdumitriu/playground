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
<jsP:getProperty name="mymap" property="email" />
<br>
<p>has been deleted from the map file.
</td>
</tr>
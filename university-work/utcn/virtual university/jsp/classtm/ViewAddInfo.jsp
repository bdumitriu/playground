<%@ page language="java" import="java.sql.*, ro.utcluj.vu.classtm.ClassTMData,
				 ro.utcluj.vu.main.IdentificationInfo,
				 java.io.StringReader,
				 java.io.BufferedReader" %>

<%
	ClassTMData data = (ClassTMData) session.getAttribute("data");
	ResultSet rs = data.getResult();

	String resourceName = new String("additional_info");

	Object tmp = session.getAttribute("id");
	IdentificationInfo id = null;
	if ((tmp != null) && (tmp instanceof IdentificationInfo))
	{
		id = (IdentificationInfo) tmp;

%>

<html>

<link rel="stylesheet" href="../../style/vu_main.css" type="text/css" />
<script language="javaScript" src="../../js/utils.js"></script>

<body>

<table class="container">

<td class="container">

<%
	if (!rs.next())
	{
%>

<h2>No additional information available at this time.</h2>


<%
	}
	else
	{
		if (rs != null)
		{
			String description = rs.getString("description");
%>
<h1>Class based additional information</h1>
<%
			if (description.equals("url"))
			{
				String url = rs.getString(resourceName);
%>
Additional information can be found at:
<a	href="<%= url %>"
	onMouseOver="windowStatus('View additional information'); return true;"
	onMouseOut="windowStatus(''); return true;">
<%= url %>
</a>
<%
			}
			else if (description.equals("txt"))
			{
				BufferedReader reader = new BufferedReader
					(new StringReader
					(rs.getString(resourceName)));

				String line;
				while ((line = reader.readLine()) != null)
				{
					out.print(line);
					out.print("<br />");
				}
				reader.close();
			}
			else if (description.equals("fil"))
			{
				StringBuffer path = new StringBuffer();
				path.append("../../data/classes/");
				path.append(id.getClassID());
				path.append("/material/");
				path.append("add_info/");
				path.append(rs.getString(resourceName));
%>
You can download additional information from here:
<a	href="<%= path.toString() %>"
	onMouseOver="windowStatus('View additional information'); return true;"
	onMouseOut="windowStatus(''); return true;">
<%= rs.getString(resourceName) %>
</a>
<%
			}else
			{
%>
<h2>Resource type unknown.</h2>
<%
			}
		}
	}
	}else
	{
%>
<h2>Session has expired.</h2>
<%
	}
%>
</td>

</table>

</body>

</html>
<%@ page language="java" import="java.sql.*,
	ro.utcluj.vu.classtm.ClassTMData" %>

<%
	ClassTMData data = (ClassTMData) session.getAttribute("data");
%>

<html>

<link rel="stylesheet" href="../../style/vu_main.css" type="text/css" />
<script language="javaScript" src="../../js/utils.js"></script>

<body>

<table class="container">
<td class="container">

<%
	ResultSet rs = data.getResult();

	if (rs != null)
	{
		if (!rs.next())
		{
%>
<h2>No class information available at this time.</h2>
<%
		}
		else
		{
%>

<h1>List of class based teaching material</h1>

<table>

<tr>
<td class="header">Resource name</td>
<td class="header">Resource type</td>
<td class="header">Resource description</td>
</tr>

<tr>
<td class="content">Schedule</td>
<%
			String schedule = rs.getString("schedule");
			String scheduleType = rs.getString("schedule_description");
			String displayedScType, dispSchedule;
			boolean existsSchedule = true;

			if ((schedule == null) && (scheduleType == null))
			{
				schedule = "N/A";
				displayedScType = "N/A";
				existsSchedule = false;
				dispSchedule = schedule;
			}
			else
			{
				if (scheduleType.equals("url"))
					displayedScType = new String("URL");
				else if (scheduleType.equals("txt"))
					displayedScType = new String("TEXT");
				else if (scheduleType.equals("fil"))
					displayedScType = new String("FILE");
				else
					displayedScType = new String("UNKNOWN");

				if (schedule.length() > 30)
					dispSchedule = schedule.substring(0, 30) + "...";
				else
					dispSchedule = schedule;
			}

%>
<td class="content"><%= displayedScType %></td>
<td class="content"><%= dispSchedule %></td>
<%
			if (existsSchedule)
			{
%>
<td class="content">
<a
	href="editResource.do?resource_name=schedule&resource_type=<%= scheduleType %>"
	onMouseOver="windowStatus('Edit schedule'); return true;"
	onMouseOut="windowStatus(''); return true;">
edit<br />schedule
</a>
<td class="content">
<a
	href="removeResource.do?resource_name=schedule&resource_type=<%= scheduleType %>
	<%
				if (scheduleType.equals("fil"))
					out.print("&resource_description=" + schedule);
	%>"
	onMouseOver="windowStatus('Remove schedule'); return true;"
	onMouseOut="windowStatus(''); return true;">
delete<br />schedule
</a>
</td>
<%
			}else
			{
%>
<td class="content">
<a
	href="../../jsp/classtm/ChooseResourceType.jsp?resource_name=schedule"
	onMouseOver="windowStatus('Create schedule'); return true;"
	onMouseOut="windowStatus(''); return true;">
create<br />schedule
</a>
</td>
<%
			}
%>
</tr>

<tr>
<td class="content">Policies</td>
<%
			String policies = rs.getString("policies");
			String policiesType = rs.getString("policies_description");
			String displayedPolType, dispPolicies;
			boolean existsPolicies = true;

			if ((policies == null) && (policiesType == null))
			{
				policies = "N/A";
				displayedPolType = "N/A";
				existsPolicies = false;
				dispPolicies = policies;
			}
			else
			{
				if (policiesType.equals("url"))
					displayedPolType = new String("URL");
				else if (policiesType.equals("txt"))
					displayedPolType = new String("TEXT");
				else if (policiesType.equals("fil"))
					displayedPolType = new String("FILE");
				else
					displayedPolType = new String("UNKNOWN");

				if (policies.length() > 30)
					dispPolicies = policies.substring(0, 30) + "...";
				else
					dispPolicies = policies;
			}

%>
<td class="content"><%= displayedPolType %></td>
<td class="content"><%= dispPolicies %></td>
<%
			if (existsPolicies)
			{
%>
<td class="content">
<a
	href="editResource.do?resource_name=policies&resource_type=<%= policiesType %>"
	onMouseOver="windowStatus('Edit policies'); return true;"
	onMouseOut="windowStatus(''); return true;">
edit<br />policies
</a>
<td class="content">
<a
	href="removeResource.do?resource_name=policies&resource_type=<%= policiesType %>
	<%
				if (policiesType.equals("fil"))
					out.print("&resource_description=" + policies);
	%>"
	onMouseOver="windowStatus('Remove policies'); return true;"
	onMouseOut="windowStatus(''); return true;">
delete<br />policies
</a>
</td>
<%
			}else
			{
%>
<td class="content">
<a
	href="../../jsp/classtm/ChooseResourceType.jsp?resource_name=policies"
	onMouseOver="windowStatus('Create policies'); return true;"
	onMouseOut="windowStatus(''); return true;">
create<br />policies
</a>
</td>
<%
			}
%>
</tr>

<tr>
<td class="content">References</td>
<%
			String references = rs.getString("references");
			String referencesType = rs.getString("references_description");
			String displayedRefType, dispReferences;
			boolean existsReferences = true;

			if ((references == null) && (referencesType == null))
			{
				references = "N/A";
				displayedRefType = "N/A";
				existsReferences = false;
				dispReferences = references;
			}
			else
			{
				if (referencesType.equals("url"))
					displayedRefType = new String("URL");
				else if (referencesType.equals("txt"))
					displayedRefType = new String("TEXT");
				else if (referencesType.equals("fil"))
					displayedRefType = new String("FILE");
				else
					displayedRefType = new String("UNKNOWN");

				if (references.length() > 30)
					dispReferences = references.substring(0, 30) +
						"...";
				else
					dispReferences = references;
			}

%>
<td class="content"><%= displayedRefType %></td>
<td class="content"><%= dispReferences %></td>
<%
			if (existsReferences)
			{
%>
<td class="content">
<a
	href="editResource.do?resource_name=references&resource_type=<%= referencesType %>"
	onMouseOver="windowStatus('Edit references'); return true;"
	onMouseOut="windowStatus(''); return true;">
edit<br />references
</a>
<td class="content">
<a
	href="removeResource.do?resource_name=references&resource_type=<%= referencesType %>
	<%
				if (referencesType.equals("fil"))
					out.print("&resource_description=" + references);
	%>"
	onMouseOver="windowStatus('Remove references'); return true;"
	onMouseOut="windowStatus(''); return true;">
delete<br />references
</a>
</td>
<%
			}else
			{
%>
<td class="content">
<a
	href="../../jsp/classtm/ChooseResourceType.jsp?resource_name=references"
	onMouseOver="windowStatus('Create schedule'); return true;"
	onMouseOut="windowStatus(''); return true;">
create<br />schedule
</a>
</td>
<%
			}
%>
</tr>

<tr>
<td class="content">Grading info</td>
<%
			String gradingInfo = rs.getString("grading_info");
			String gradingInfoType = rs.getString("grading_info_description");
			String displayedGrdType, displayedGradingInfo;
			boolean existsGradingInfo = true;

			if ((gradingInfo == null) && (gradingInfoType == null))
			{
				gradingInfo = "N/A";
				displayedGrdType = "N/A";
				existsGradingInfo = false;
				displayedGradingInfo = gradingInfo;
			}
			else
			{
				if (gradingInfoType.equals("url"))
					displayedGrdType = new String("URL");
				else if (gradingInfoType.equals("txt"))
					displayedGrdType = new String("TEXT");
				else if (gradingInfoType.equals("fil"))
					displayedGrdType = new String("FILE");
				else
					displayedGrdType = new String("UNKNOWN");

				if (gradingInfo.length() > 30)
					displayedGradingInfo = gradingInfo.substring(0, 30) +
						"...";
				else
					displayedGradingInfo = gradingInfo;
			}

%>
<td class="content"><%= displayedGrdType %></td>
<td class="content"><%= displayedGradingInfo %></td>
<%
			if (existsGradingInfo)
			{
%>
<td class="content">
<a
	href="editResource.do?resource_name=grading_info&resource_type=<%= gradingInfoType %>"
	onMouseOver="windowStatus('Edit grading info'); return true;"
	onMouseOut="windowStatus(''); return true;">
edit<br />grading info
</a>
<td class="content">
<a
	href="removeResource.do?resource_name=grading_info&resource_type=<%= gradingInfoType %>
	<%
				if (gradingInfoType.equals("fil"))
					out.print("&resource_description=" + gradingInfo);
	%>"
	onMouseOver="windowStatus('Remove grading info'); return true;"
	onMouseOut="windowStatus(''); return true;">
delete<br />grading info
</a>
</td>
<%
			}else
			{
%>
<td class="content">
<a
	href="../../jsp/classtm/ChooseResourceType.jsp?resource_name=grading_info"
	onMouseOver="windowStatus('Create grading info'); return true;"
	onMouseOut="windowStatus(''); return true;">
create<br />grading info
</a>
</td>
<%
			}
%>
</tr>

<tr>
<td class="content">Additional info</td>
<%
			String addInfo = rs.getString("additional_info");
			String addInfoType = rs.getString("additional_info_description");
			String displayedAddType;
			String displayedAddInfo;
			boolean existsAddInfo = true;

			if ((addInfo == null) && (addInfoType == null))
			{
				addInfo = "N/A";
				displayedAddType = "N/A";
				existsAddInfo = false;
				displayedAddInfo = addInfo;
			}
			else
			{
				if (addInfoType.equals("url"))
					displayedAddType = new String("URL");
				else if (addInfoType.equals("txt"))
					displayedAddType = new String("TEXT");
				else if (addInfoType.equals("fil"))
					displayedAddType = new String("FILE");
				else
					displayedAddType = new String("UNKNOWN");

				if (addInfo.length() > 30)
					displayedAddInfo = addInfo.substring(0, 30) +
						"...";
				else
					displayedAddInfo = addInfo;
			}

%>
<td class="content"><%= displayedAddType %></td>
<td class="content"><%= displayedAddInfo %></td>
<%
			if (existsAddInfo)
			{
%>
<td class="content">
<a
	href="editResource.do?resource_name=add_info&resource_type=<%= addInfoType %>"
	onMouseOver="windowStatus('Edit additional info'); return true;"
	onMouseOut="windowStatus(''); return true;">
edit<br />additional info
</a>
<td class="content">
<a
	href="removeResource.do?resource_name=add_info&resource_type=<%= addInfoType %>
	<%
				if (addInfoType.equals("fil"))
					out.print("&resource_description=" + addInfo);
	%>"
	onMouseOver="windowStatus('Remove additional info'); return true;"
	onMouseOut="windowStatus(''); return true;">
delete<br />additional info
</a>
</td>
<%
			}else
			{
%>
<td class="content">
<a
	href="../../jsp/classtm/ChooseResourceType.jsp?resource_name=add_info"
	onMouseOver="windowStatus('Create additional info'); return true;"
	onMouseOut="windowStatus(''); return true;">
create<br />additional info
</a>
</td>
<%
			}
%>
</tr>


</table>
<%
		}
	}
%>

</td>
</table>

</body>

</html>
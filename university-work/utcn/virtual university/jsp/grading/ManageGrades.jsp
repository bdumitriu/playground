<%@ page language="java" import="java.sql.*,
				 ro.utcluj.vu.grading.GradingData,
				 java.util.ArrayList,
				 ro.utcluj.vu.grading.GradebookParser,
				 java.util.Map" %>



<%
	GradingData data = (GradingData) session.getAttribute("data");
%>

<html>

<link rel="stylesheet" href="../../style/vu_main.css" type="text/css" />
<script language="javaScript" src="../../js/utils.js"></script>

<body>

<table class="container">

<td class="container">

<%
	ResultSet rs = data.getResult();
	if ((!rs.next()) || (rs == null))
	{
%>

<h2>No grade columns available at this time.</h2>

<%
	}
	else
	{
%>
<h2> Gradebook management </h2>
<%
		GradebookParser processor = new GradebookParser(rs);
		ArrayList columnListOfIDs = processor.getColumnAttributes()[0];
		ArrayList columnListOfNames = processor.getColumnAttributes()[1];
		ArrayList studentListOfIDs = processor.getStudentIDs();

		Map grades = processor.getGrades();
		Map students = processor.getStudents();
		Map columns = processor.getColumns();

		String columnName;
%>
<table>
<table>

<tr>
<td class="header">Student Name</td>

<%
		for (int i = 0; i < columnListOfNames.size(); i++)
		{
			columnName = (String) columnListOfNames.get(i);
%>
<td class="header"><%= columnName %></td>
<%
		}
%>

<%

		Map studentGrade;
		Map gradeColumns;
		String studentID;
		String studentName;
		String columnID;
		String grade;

		for (int i = 0; i < studentListOfIDs.size(); i++)
		{
			studentID = (String) studentListOfIDs.get(i);
			studentName = (String) students.get(studentID);
			studentGrade = (Map) grades.get(studentID);
%>
<tr>
<td class="content"><%= studentName %></td>
<%
			// seek on the columnList

                        for (int j = 0; j < columnListOfIDs.size(); j++)
			{
				columnID = (String) columnListOfIDs.get(j);
				columnName = (String) columns.get(columnID);
				grade = (String) studentGrade.get(columnID);
				if (grade == null)
				{
					grade = "NO GRADE";
%>
<td class="content"><%= grade %> <br />
	<a
		href="../../jsp/grading/NewGradePage.jsp?student_ID=<%= studentID %>&column_ID=<%= columnID %>&student_name=<%= studentName %>&column_name=<%= columnName %>"
		onMouseOver="windowStatus('Create new grade'); return true;"
		onMouseOut="windowStatus(''); return true;">
	New grade
	</a>
</td>
<%
				}
				else
				{
%>
<td class="content"><%= grade %> <br />
	<a
		href="removeGrade.do?student_ID=<%= studentID %>&column_ID=<%= columnID %>"
		onMouseOver="windowStatus('Delete grade'); return true;"
		onMouseOut="windowStatus(''); return true;">
	Delete grade
	</a>
	<br />
	<a
		href="../../jsp/grading/EditGradePage.jsp?student_ID=<%= studentID %>&column_ID=<%= columnID %>&grade=<%= grade %>&student_name=<%= studentName %>&column_name=<%= columnName %>"
		onMouseOver="windowStatus('Edit grade'); return true;"
		onMouseOut="windowStatus(''); return true;">
	Edit grade
	</a>
<%
				}
			}
%>
</tr>
<%
		}
%>

<%
	}
%>
</td>
</table>

</body>

</html>
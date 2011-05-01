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
<h2> The class gradebook </h2>
<%
		GradebookParser processor = new GradebookParser(rs);
		ArrayList columnListOfIDs = processor.getColumnAttributes()[0];
		ArrayList columnListOfNames = processor.getColumnAttributes()[1];
		ArrayList studentListOfIDs = processor.getStudentIDs();

		Map grades = processor.getGrades();
		Map students = processor.getStudents();

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
				grade = (String) studentGrade.get(columnID);
				if (grade == null)
					grade = "NO GRADE";
%>
<td class="content"><%= grade %></td>
<%
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
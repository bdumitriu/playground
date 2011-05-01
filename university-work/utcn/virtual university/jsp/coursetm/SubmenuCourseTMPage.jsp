<%@ page language="java" import="java.sql.*, java.util.*, ro.utcluj.vu.coursetm.CourseTMData,
				 ro.utcluj.vu.main.IdentificationInfo,
				 ro.utcluj.vu.error.SessionExpiredException,
				 ro.utcluj.vu.error.InvalidDataException" %>

<%
	CourseTMData data = (CourseTMData) session.getAttribute("data");

	ResultSet rs = data.getResult();
	rs.next();
%>

<html>

<link rel="stylesheet" href="../../style/vu_main.css" type="text/css" />
<link rel="stylesheet" href="../../style/main_menu.css" type="text/css" />
<script language="javaScript" src="../../js/utils.js"></script>

<body>

<table class="menu">
<td class="menu">
<br />
<br />
<a	class="menu"
	href="../../html/teacher/menu.html"
	onMouseOver="windowStatus('Course material'); return true;"
	onMouseOut="windowStatus(''); return true;">
Course material
</a>
<br />

<img id="img1" src="../../img/arrow_r1.jpg" />
<a	class="submenu"
	href="<%= "../../data/courses/" + rs.getString("course_ID") + "/" + rs.getString("start_page") %>"
	target="main"
	onMouseOver="windowStatus('View course'); chImg(img1, '../../img/arrow_r2.jpg'); return true;"
	onMouseOut="windowStatus(''); chImg(img1, '../../img/arrow_r1.jpg'); return true;">
View course
</a>
<br />

<img id="img2" src="../../img/arrow_r1.jpg" />
<a	class="submenu"
	href="<%= "../../data/courses/" + rs.getString("course_ID") + "/" + rs.getString("description") %>"
	target="main"
	onMouseOver="windowStatus('View course description'); chImg(img2, '../../img/arrow_r2.jpg'); return true;"
	onMouseOut="windowStatus(''); chImg(img2, '../../img/arrow_r1.jpg'); return true;">
View course description
</a>
<br />

<img id="img3" src="../../img/arrow_r1.jpg" />
<a	class="submenu"
	href="<%= "../../data/courses/" + rs.getString("course_ID") + "/" + rs.getString("content") %>"
	target="main"
	onMouseOver="windowStatus('View course content'); chImg(img3, '../../img/arrow_r2.jpg'); return true;"
	onMouseOut="windowStatus(''); chImg(img3, '../../img/arrow_r1.jpg'); return true;">
View course content
</a>
<br />

<img id="img4" src="../../img/arrow_r1.jpg" />
<a	class="submenu"
	href="<%= "../../data/courses/" + rs.getString("course_ID") + "/" + rs.getString("bibliography") %>"
	target="main"
	onMouseOver="windowStatus('View course bibliography'); chImg(img4, '../../img/arrow_r2.jpg'); return true;"
	onMouseOut="windowStatus(''); chImg(img4, '../../img/arrow_r1.jpg'); return true;">
View course bibliography
</a>
<br />

<img id="img5" src="../../img/arrow_r1.jpg" />
<a	class="submenu"
	href="<%= "../../data/courses/" + rs.getString("course_ID") + "/" + rs.getString("objectives") %>"
	target="main"
	onMouseOver="windowStatus('View course objectives'); chImg(img5, '../../img/arrow_r2.jpg'); return true;"
	onMouseOut="windowStatus(''); chImg(img5, '../../img/arrow_r1.jpg'); return true;">
View course objectives
</a>
<br />

<img id="img6" src="../../img/arrow_r1.jpg" />
<a	class="submenu"
	href="<%= "../../data/courses/" + rs.getString("course_ID") + "/" + rs.getString("prerequisites") %>"
	target="main"
	onMouseOver="windowStatus('View course prerequisites'); chImg(img6, '../../img/arrow_r2.jpg'); return true;"
	onMouseOut="windowStatus(''); chImg(img6, '../../img/arrow_r1.jpg'); return true;">
View course prerequisites
</a>
<br />

<%
	if (data.getPosition().equals("teacher"))
	{
%>

<img id="img7" src="../../img/arrow_r1.jpg" />
<a	class="submenu"
	href="getCourseAbout.do?course_name=<%= rs.getString("name") %>&course_author=<%= rs.getString("first_name") + " " + rs.getString("last_name").toUpperCase() %>&author_email=<%= rs.getString("e_mail") %>"
	target="main"
	onMouseOver="windowStatus('About the course'); chImg(img7, '../../img/arrow_r2.jpg'); return true;"
	onMouseOut="windowStatus(''); chImg(img7, '../../img/arrow_r1.jpg'); return true;">
About the course
</a>
<br />

<%
	}
%>

<a	class="menu"
	href="../../html/teacher/submenu_classtm.html"
	onMouseOver="windowStatus('Class material'); return true;"
	onMouseOut="windowStatus(''); return true;">
Class material
</a>
<br /><br />

<a	class="menu"
	href="../../html/teacher/submenu_ann.html"
	onMouseOver="windowStatus('Announcements'); return true;"
	onMouseOut="windowStatus(''); return true;">
Announcements
</a>
<br />

<a	class="menu"
	href="../../html/teacher/submenu_email.html"
	onMouseOver="windowStatus('E-mail'); return true;"
	onMouseOut="windowStatus(''); return true;">
E-mail
</a>
<br />

<a	class="menu"
	href="../../html/teacher/submenu_conf.html"
	onMouseOver="windowStatus('Conferences'); return true;"
	onMouseOut="windowStatus(''); return true;">
Conferences
</a>
<br />

<a	class="menu"
	href="../../html/teacher/submenu_chat.html"
	onMouseOver="windowStatus('Chat'); return true;"
	onMouseOut="windowStatus(''); return true;">
Chat
</a>
<br /><br />

<a	class="menu"
	href="submenu_taskman.html"
	onMouseOver="windowStatus('Task management'); return true;"
	onMouseOut="windowStatus(''); return true;">
Task management
</a>
<br />

<a	class="menu"
	href="../../html/teacher/submenu_taskass.html"
	onMouseOver="windowStatus('Task assignment'); return true;"
	onMouseOut="windowStatus(''); return true;">
Task assignment
</a>
<br />

<a	class="menu"
	href="../../html/teacher/submenu_grading.html"
	onMouseOver="windowStatus('Grading book'); return true;"
	onMouseOut="windowStatus(''); return true;">
Grading book
</a>
<br />

</td>
</table>

</body>

</html>
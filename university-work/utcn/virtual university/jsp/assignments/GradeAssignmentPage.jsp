<%@ page language="java" %>

<html>

<link rel="stylesheet" href="../../style/vu_main.css" type="text/css" />
<script language="javaScript" src="../../js/utils.js"></script>

<body>

<table class="container">
<td class="container">

<h1>
Grade assignment
</h1>

<table border="0">

<form action="gradeAss.do" method="post">

<input type="hidden" name="class_ID" value="<%= request.getParameter("class_ID") %>">
<input type="hidden" name="idx" value="<%= request.getParameter("idx") %>">
<input type="hidden" name="status" value="<%= request.getParameter("status") %>">

<tr>
<td align="right" width="40%">
<img	src="../../img/help.jpg"
	onMouseOver="windowStatus('Here is where you award a grade to the student for the current assignment.'); return true;"
	onMouseOut="windowStatus(''); return true;">
Grade:
</td>
<td align="left">
<input class="custom" type="text" name="grade" />
</td>
</tr>

<tr>
<td align="right">
<br />
<img	src="../../img/help.jpg"
	onMouseOver="windowStatus('You can optionally add a comment to the grade here.'); return true;"
	onMouseOut="windowStatus(''); return true;">
Grade comments:
</td>
<td align="left">
<br />
<textarea class="custom" name="gradeCom" rows="5" cols="40"></textarea>
</td>
</tr>

<tr>
<td align="center" colspan="2">
<br />
<input class="custom" type="reset" value=" << Reset >> " />
<input class="custom" type="submit" value=" << Submit >> " />
</td>
</tr>

</form>

</table>

<br /><br /><br />
Move your mouse over the <img src="../../img/help.jpg"> image to get help in the status bar.

</td>
</table>

</body>

</html>
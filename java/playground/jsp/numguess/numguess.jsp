<%@ page import="numguess.NumberGuess" %>

<jsp:useBean id="nguess" class="numguess.NumberGuess" scope="session" />

<jsp:setProperty name="nguess" property="*" />

<html>
<head>
<title>Bogdan's Number Guess</title>
</head>

<body bgcolor="white">

<font size=4>

<%
	if (nguess.getSuccess()) {
%>

Damn! You got it!<br>
And it took you <%= nguess.getNumGuesses() %> tries. That's a lot!<br>
<h1>YOU SHOULD BE ASHAMED OF YOURSELF!!!</h1>

<% 
		nguess.reset();
%>

Care to <a href="numguess.jsp">make a fool of yourself again?</a>

<%
	} else {
		if (!(nguess.getHint().equals("null"))) {
%>

Missed! Ha ha! Try <b><%= nguess.getHint() %></b>.

<%
		}
%>

You have made <%= nguess.getNumGuesses() %> 

<%
		if (nguess.getNumGuesses() != 1) {
%>

guesses

<%
		} else {
%>

guess

<%
		}
%>

already.<br>
I'd quit if I were you!<br>

I'm thinking of a number between 1 and 100.<p>

<form method=get>
What's your guess? <input type=text name=guess>
<input type=submit value="Submit">
</form>

<%
	}
%>

</font>
</body>
</html>
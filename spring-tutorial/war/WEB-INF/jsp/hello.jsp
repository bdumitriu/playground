<%@ include file="/WEB-INF/jsp/include.jsp" %>

<html>
<head>
<meta http-equiv="Content-Type" content="text/html; charset=ISO-8859-1">
<title><fmt:message key="title" /></title>
</head>

<body>

<h1><fmt:message key="heading" /></h1>
<p>
<fmt:message key="greeting">
	<fmt:param value="${model.now}" />
</fmt:message>
</p>

<h3>Products</h3>
<c:forEach items="${model.products}" var="product">
	<c:out value="${product.description}" />
	<i>&euro;<c:out value="${product.price}" /></i>
	<br />
</c:forEach>

<br />
<a href="<c:url value="priceincrease.htm" />">Increase Prices</a>

</body>
</html>

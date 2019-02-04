<%@ page language="java" contentType="text/html; charset=UTF-8"
    pageEncoding="UTF-8"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<!DOCTYPE html><html><head><meta charset="UTF-8">
<title>Insert title here</title>
</head>
<body>
<c:if test="${id==null || id==''}">
	<script type="text/javascript">
		//alert("${id}");
		location.href="../main/main.jsp";
	</script>	
</c:if>
<c:if test="${id!=null && id!='' }">
	<script type="text/javascript">
		//alert("\n${id}");
		location.href="../kmember/main_member.jsp";
	</script>	
</c:if>
</body>
</html>
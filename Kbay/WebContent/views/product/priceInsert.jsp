<%@ page language="java" contentType="text/html; charset=UTF-8"
    pageEncoding="UTF-8"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<!DOCTYPE html>
<html>
<head>
<meta charset="UTF-8">
<title>Insert title here</title>
</head>
<body>
	<c:if test="${result > 0 }">
		<script type="text/javascript">
			alert("Bidding에 참여하였습니다.");
			location.href = "myBidding.do?gubun=2&pageNum=${pageNum}";
		</script>		
	</c:if>
	
	<c:if test="${result == 0 }">
		<script type="text/javascript">
			alert("Bidding 참여 중 에러발생");
			history.go(-1);
		</script>
	</c:if>
</body>
</html>
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
<c:if test="${result > 0}">
	<script type="text/javascript">
		alert("상품 삭제가 정상적으로 처리되었습니다.");
		location.href="AdminProductListView.do?class_id1=${p_class_id1}&class_id2=${p_class_id2}&pageNum=${pageNum}";
	</script>
</c:if>
<c:if test="${result == 0 }">
	<script type="text/javascript">
		alert("상품 삭제를 하지 못하였습니다.");
		history.go(-1);
	</script>
</c:if>
</body>
</html>
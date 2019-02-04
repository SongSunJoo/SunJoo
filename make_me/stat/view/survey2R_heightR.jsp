<%@ page language="java" contentType="text/html; charset=UTF-8"
    pageEncoding="UTF-8"%>
<!DOCTYPE html>
<html>
<head>
<meta charset="UTF-8">
<title>Insert title here</title>
</head>
<body>
<%-- <input type="hidden" name="user_id" value="${user_id }">
<input type="hidden" name="height_value" value="${height_value }"> --%>
<script type="text/javascript">
	/* alert("before R controller..."); */
	location.href = "survey2R_heightR.do?user_id=${user_id }&height_value=${height_value }";
</script>

</body>

</html>
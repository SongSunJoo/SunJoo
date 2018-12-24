<%@ page language="java" contentType="text/html; charset=UTF-8"
    pageEncoding="UTF-8"%>
<%@ include file="sessionChk.jsp" %>
<!DOCTYPE html>
<html>
<head>
<meta charset="UTF-8">
<title>Insert title here</title>
</head>
<body>
<h2>${Kmember.id}님 반갑습니다</h2>
<table border="1">
	<tr><td width="120" align="center"><a href="myPageView.do">조회</a></td>
		<td width="120" align="center"><a href="updateForm.do">수정</a></td>
		<td width="120" align="center"><a href="logout.do">로그아웃</a></td></tr>
</table>
</form>
</body>
</html>
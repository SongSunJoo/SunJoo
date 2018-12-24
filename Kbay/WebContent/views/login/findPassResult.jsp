<%@ page language="java" contentType="text/html; charset=UTF-8"
    pageEncoding="UTF-8"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<!DOCTYPE html>
<html>
<head>
<meta charset="UTF-8">
<meta http-equiv="X-UA-Compatible" content="IE=edge">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Insert title here</title>
<link href="../../css/bootstrap.min.css" rel="stylesheet">
<script type="text/javascript" src="../../js/jquery.js"></script>
<script src="../../js/bootstrap.min.js"></script>
</head>
<body>
<div class="container">
	<div id="content" align="center">
		<h3>암호찾기 결과</h3>
		<table class="table table-hover">
			<c:if test="${kme.id != null }">
				<tr><th> ${kme.id }의 비밀번호는 </th></tr>
				<tr><th>${kme.pass }입니다</th></tr>
				<tr><th colspan=2><input type="button" class="btn btn-default" value="창닫기" onclick="self.close()"></th></tr>
			</c:if>
			<c:if test="${kme.id == null }"> 
				<tr><th>존재하지 않는 회원 정보입니다.</th></tr>
				<tr><th colspan=2><input type="button" class="btn btn-default" value="창닫기" onclick="self.close()"></th></tr>
			</c:if>
		</table>
	</div>
</div>
</body>
</html>
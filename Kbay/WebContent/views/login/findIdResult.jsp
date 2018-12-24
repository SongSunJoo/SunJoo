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
		<h3>아이디 찾기 결과</h3>
		<table class="table table-hover">
			<!-- 만약에 kme의 tel값이 null값이 아니면, tel이 있을 때 -->
			<c:if test="${kme.tel != null }">
				<tr><th> ${kme.tel }의 아이디는 </th></tr>
				<tr><th>${kme.id }입니다</th></tr>
				<tr><th colspan=2><input type="button" class="btn btn-default" value="창닫기" onclick="self.close()"></th></tr>
			</c:if>
			<c:if test="${kme.tel == null }"> 
				<tr><th>존재하지 않는 회원 정보입니다.</th></tr>
				<tr><th colspan=2><input type="button" class="btn btn-default" value="창닫기" onclick="self.close()"></th></tr>
			</c:if>
		</table>
	</div>
</div>
</body>
</html>
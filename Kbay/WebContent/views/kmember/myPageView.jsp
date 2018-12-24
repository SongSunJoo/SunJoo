<%@ page language="java" contentType="text/html; charset=UTF-8"
    pageEncoding="UTF-8"%>
<%-- <%@ include file="../../sessionChk.jsp" %> --%>
<!DOCTYPE html>
<html>
<head>
<meta charset="UTF-8">
<meta http-equiv="X-UA-Compatible" content="IE=edge">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>경매 상품 : 믿을 수 있는 경매, Kbay</title>
<link href="../../css/bootstrap.min.css" rel="stylesheet">
<script type="text/javascript" src="../../js/jquery.js"></script>
<script src="../../js/bootstrap.min.js"></script>
</head>
<body>
<div class="container">
	<div class="col-md-3"></div>
	<div class="col-md-7">
		<table border="1" class="table table-bordered">
			<h4 align="center">회원정보 보기</h4><br>
				<tr><th width="30%">아이디</th><td width="70%">${Kmember.id}</td></tr>
				<tr><th>이름</th><td>${Kmember.name}</td></tr>
				<tr><th>전화번호</th><td>${Kmember.tel}</td></tr>
				<tr><th>이메일</th><td>${Kmember.email}</td></tr>
				<tr><th>주소</th><td>${Kmember.addr}</td></tr>
				<tr><th>판매 패널티</th><td>${Kmember.sell_penalty}</td></tr>
				<tr><th>구입 패널티</th><td>${Kmember.buy_penalty}</td></tr>
				<tr><th>가입 날짜</th><td>${Kmember.reg_date}</td></tr>
				</table>
			<div class="row" align="center">
				<a href="updateForm.do" class="btn btn-default" role="button">회원수정</a>
				<a href="../main/Main.do" class="btn btn-default" role="button">메인으로 가기</a>
			</div>
	<div class="col-md-3"></div>
	</div>
</div>
</body>
</html>


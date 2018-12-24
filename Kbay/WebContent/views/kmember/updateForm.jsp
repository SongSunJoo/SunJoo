<%@ page language="java" contentType="text/html; charset=UTF-8"
    pageEncoding="UTF-8"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
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
<script type="text/javascript">
	// pass 체크
	function passConfirm() {
		if (frm.pass.value != frm.newpass.value) {
			alert("암호와 암호 확인이 다릅니다");
			frm.pass.focus();	
			return false;
	// 실패했을때 return
		}
	}
</script>
</head>
<body>
<div class="container">
	<div class="col-md-3"></div>
	<div class="col-md-7">
	<form action="update.do" method="post" name="frm" onsubmit="return passConfirm()">
		<input type="hidden" name="id" value="${Kmember.id}">
			<table border="1" class="table table-bordered">
			<h4 align="center">회원정보 수정</h4><br>
				<tr><th width="30%">아이디</th><td width="70%">${Kmember.id}</td></tr>
				<tr><th>비밀번호</th>
					<td><input type="password" name="pass" required="required" autofocus="autofocus"></td></tr>
				<tr><th>비밀번호확인</th>
					<td><input type="password" name="newpass" required="required"></td></tr>
				<tr><th>이름</th><td><input type="text" name="name" required="required" value="${Kmember.name}"></td></tr>
				<tr><th>이메일</th><td><input type="text" name="email" required="required" value="${Kmember.email}"></td></tr>
				<tr><th>전화</th><td>${Kmember.tel}</td></tr>
				<tr><th>주소</th><td><input type="text" name="addr" required="required" value="${Kmember.addr}"></td></tr>
			</table>
			<div class="row" align="center">
				<input type="submit" class="btn btn-default" value="수정완료">
				<a href="../login/delete.do?id=${Kmember.id}" class="btn btn-default" role="button" onclick="return confirm('탈퇴하시겠습니까?')">탈퇴하기</a>
				<a href="../main/Main.do" class="btn btn-default" role="button">메인으로 가기</a>
			</div>
	</form>
	</div>
	<div class="col-md-3"></div>
	</div>
</body>
</html>

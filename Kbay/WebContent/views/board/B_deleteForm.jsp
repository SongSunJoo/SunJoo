<%@ page language="java" contentType="text/html; charset=UTF-8"
    pageEncoding="UTF-8"%>
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
	function chk() {
		if (frm.b_pass.value != frm.b_pass2.value) {
			alert("암호가 다릅니다");
			frm.pass2.value="";
			frm.pass2.focus();
			return false;
		}
	}
</script>
</head>
<body>
<div class="container">
	<div class="col-md-4"></div>
	<div class="col-md-4">
		<form action="B_delete.do" name="frm" onsubmit="return chk()">
			<input type="hidden" name="b_pass" value="${kboard.b_pass}">
			<input type="hidden" name="b_num" value="${kboard.b_num}">
			<input type="hidden" name="pageNum" value="${pageNum}">
		<table class="table table-bordered">
			<h4 align="center">게시글 삭제</h4><br>
			<tr><th>암호</th><td><input type="password" name="b_pass2"
				required="required" autofocus="autofocus"></td></tr>
			<tr><th colspan="2">
			<div class="row" align="center">
				<input type="submit" class="btn btn-default" value="확인">
				<input type="button" class="btn btn-default" value="뒤로가기" onclick="history.go(-1)">
			</th></tr></div>
		</table></form>
	</div>
	<div class="col-md-4"></div>
	</div>
</body>
</html>


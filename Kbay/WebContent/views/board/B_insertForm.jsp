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
	$(function() {
		$('#btn_ok').click(function() {
		
			$('#frm').attr('action', 'B_insert.do').submit();
		});
	});
</script>
</head>
<body>
<div class="container">
	<div class="col-md-2"></div>
	<div class="col-md-8">
		<form action="" id="frm" method="post">
			<input type="hidden" name="id" 			value="${id}">
			<input type="hidden" name="b_num" 		value="${b_num}">
			<input type="hidden" name="b_ref" 		value="${b_ref}">
			<input type="hidden" name="b_re_level" 	value="${b_re_level}">
			<input type="hidden" name="b_re_step" 	value="${b_re_step}">
			<input type="hidden" name="pageNum" 	value="${pageNum}">
				<table border="1" class="table table-bordered">
					<h4>게시글 입력</h4>
					<tr><th width="20%">제목</th>
						<td width="80%"><input type="text" name="b_subject"
							required="required" autofocus="autofocus"></td></tr>
					<tr><th>작성자</th>
						<td>${id}</td></tr>
					<tr><th>내용</th>
						<td><textarea rows="15" cols="80" name="b_content" required="required"></textarea></td></tr>
					<tr><th>암호</th><td><input type="password" name="b_pass" required="required"></td></tr>
				</table>
				<div class="row" align="center">
				<input type="submit" class="btn btn-default" id="btn_ok" value="확인">
				<input type="button" class="btn btn-default" value="뒤로가기" onclick="history.go(-1)">
			</div>
		</form>
	<div class="col-md-2"></div>
	</div>
	</div>

</body>
</html>

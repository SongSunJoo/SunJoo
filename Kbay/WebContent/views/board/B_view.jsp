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
</head>
<body>
<div class="container">
	<div class="col-md-2"></div>
	<div class="col-md-8">
		<table border="1" class="table table-bordered">
			<h4 align="center">게시글 보기</h4><br>
			<tr><th width="100">번호</th><td width="350">${kboard.b_num}</td></tr>
			<tr><th>제목</th><td>${kboard.b_subject}</td></tr>
			<tr><th>작성자</th><td>${kboard.id}</td></tr>
			<tr><th>내용</th><td>${kboard.b_content}</td></tr>
			<tr><th>조회수</th><td>${kboard.b_readcount}</td></tr>
			<tr><th>작성일</th><td>${kboard.b_reg_date}</td></tr>
		</table>
		<div class="row" align="center">
		<a href="B_list.do?pageNum=${pageNum}">목록</a>
		<a href="B_updateForm.do?b_num=${kboard.b_num}&pageNum=${pageNum}">수정</a>
		<a href="B_deleteForm.do?b_num=${kboard.b_num}&pageNum=${pageNum}">삭제</a>
		<a href="B_insertForm.do?b_num=${kboard.b_num}&pageNum=${pageNum}">답변</a>
		</div>
	</div>
	<div class="col-md-2"></div>
</div>
</body>
</html>

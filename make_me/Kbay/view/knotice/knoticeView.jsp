<%@ page language="java" contentType="text/html; charset=UTF-8"
    pageEncoding="UTF-8"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>    
<!DOCTYPE html>
<html>
<head>
<meta charset="UTF-8">
<meta http-equiv="X-UA-Compatible" content="IE=edge">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>공지사항 상세 : 믿을 수 있는 경매, Kbay</title>
</head>
<body>
<h3 align="center">공지사항</h3>
<hr>
	<div class="row">
			<div class="col-md-3"></div>
			<div class="col-md-5">
			<div id="content" align="center">
			<table class="table table-hover">
				<caption>상세 내용</caption>
				<tr><th>번호</th><td>${knotice.n_num }</td></tr>
				<tr><th>제목</th><td>${knotice.n_subject }</td></tr>
				<tr><th>작성자</th><td>${knotice.m_id }</td></tr>
				<tr><th>내용</th><td>${knotice.n_content }</td></tr>
				<tr><th>등록일</th><td>${knotice.n_reg_date }</td></tr>
			</table>
			<div align="center">
				<a href="KnoticeList.do">목록</a>
			</div>
			<div class="col-md-4"></div>
			</div>
		</div>
	</div>
</body>
</html>

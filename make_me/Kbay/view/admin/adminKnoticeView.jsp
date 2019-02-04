<%@ page language="java" contentType="text/html; charset=UTF-8"
    pageEncoding="UTF-8"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>    
<!DOCTYPE html>
<html>
<head>
<meta charset="UTF-8">
<title>Insert title here</title>
</head>
<body>
<h3 align="center">공지사항</h3>
<hr>
<table border="1">
	<caption>상세 내용</caption>
	<tr><th>번호</th><td>${knotice.n_num }</td></tr>
	<tr><th>제목</th><td>${knotice.n_subject }</td></tr>
	<tr><th>작성자</th><td>${knotice.m_id }</td></tr>
	<tr><th>내용</th><td>${knotice.n_content }</td></tr>
	<tr><th>등록일</th><td>${knotice.n_reg_date }</td></tr>
</table>
<div align="center">
	<a href="AdminKnoticeList.do">목록</a>
</div>
</body>
</html>
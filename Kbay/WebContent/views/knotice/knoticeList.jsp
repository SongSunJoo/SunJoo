<%@ page language="java" contentType="text/html; charset=UTF-8"
    pageEncoding="UTF-8"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<!DOCTYPE html>
<html>
<head>
<meta charset="UTF-8">
<meta charset="UTF-8">
<meta http-equiv="X-UA-Compatible" content="IE=edge">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>공지사항 : 믿을 수 있는 경매, Kbay</title>
<link href="../../css/bootstrap.min.css" rel="stylesheet">
<script type="text/javascript" src="../../js/jquery.js"></script>
<script src="../../js/bootstrap.min.js"></script>
</head>
<body>
<h3 align="center">공지사항</h3>
<hr>
	<div class="container">
	<div id="content" align="center">
	<table class="table table-hover">
		<tr>
			<th width="10%">번호</th>
			<th width="10%">제목</th>
			<th width="10%">작성자</th>
			<th width="10%">등록일</th>
		</tr>
		<c:if test="${empty list }">
		<tr>
			<td colspan="4" align="center">등록된 공지사항이 없습니다.</td>
		</tr>
		</c:if>
		<c:if test="${not empty list }">
			<c:forEach var="knotice" items="${list }">
				<tr>
					<c:set var="total" value="${total }"></c:set>
					<td>${total }<c:set var="total" value="${total-1 }" /></td>
					<td><a href="KnoticeView.do?n_num=${knotice.n_num }&pageNum=${currentPage}">${knotice.n_subject }</a></td>
					<td>${knotice.m_id }</td>
					<td>${knotice.n_reg_date }</td>
				</tr>
			</c:forEach>		
		</c:if>
	</table>
	<div align="center">
		<c:if test="${startPage > pagePerBlock }">
			<a href="KnoticeList.do?pageNum=${startPage - 1 }">[이전]</a>
		</c:if>
		<c:forEach var="i" begin="${startPage }" end="${endPage }">
			<a href="KnoticeList.do?pageNum=${i }">${i }</a>
		</c:forEach>
		<c:if test="${endPage < totPage }">
			<a href="KnoticeList.do?pageNum=${endPage + 1 }">[다음]</a>
		</c:if>
	</div>
	</div>
	</div>
	</div>
</body>
</html>

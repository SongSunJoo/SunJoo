<%@ page language="java" contentType="text/html; charset=UTF-8"
    pageEncoding="UTF-8"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<!DOCTYPE html>
<html>
<head>
<meta charset="UTF-8">
<meta http-equiv="X-UA-Compatible" content="IE=edge">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>상품 검색 : 믿을 수 있는 경매, Kbay</title>
<link href="../../css/bootstrap.min.css" rel="stylesheet">
<script type="text/javascript" src="../../js/jquery.js"></script>
<script src="../../js/bootstrap.min.js"></script>
</head>
<body>
	<div class="col-md-12">
	<div id="content" align="center">
	<table class="table table-hover">
		<tr>
			<th width="10%">번호</th>
			<th width="10%">상품명</th>
			<th width="10%">대분류</th>
			<th width="10%">소분류</th>
			<th width="10%">가격</th>
			<th width="10%">사용여부</th>
			<th width="10%">경매시작일</th>
			<th width="10%">경매마감일</th>
		</tr>
		<c:if test="${ empty searchList }">
			<tr>
				<td colspan="8" align="center">조회된 상품이 없습니다</th>
			</tr>
		</c:if>
		<c:if test="${ not empty searchList }">
			<c:forEach var="search" items="${searchList }">
				<tr>
					<c:set var="searchTotal" value="${ searchTotal }"></c:set>
					<td>${ searchTotal }<c:set var="searchTotal" value="${ searchTotal-1 }"/></td>
					<td><a href="../product/productDetail.do?p_num=${search.p_num }&gubun=1&pageNum=${currentPage}">${search.p_name }</a></td>
					<td>${search.p_class_name1 }</td>
					<td>${search.p_class_name2 }</td>
					<td>${search.p_price }</td>
					<td>${search.p_used }</td>
					<td>${search.p_start_date }</td>
					<td>${search.p_end_date }</td>
				</tr>
			</c:forEach>
		</c:if>
	</table>
	<div align="center">
		<c:if test="${startPage > pagePerBlock }">
			<a href="../search/search.do?word=${param.word }&pageNum=${startPage-1 }">[이전]</a>
		</c:if>		
		<c:forEach var="i" begin="${startPage }" end="${endPage }">
			<a href="../search/search.do?word=${param.word }&pageNum=${i }">${i }</a>
		</c:forEach>
		<c:if test="${endPage < totPage }">
			<a href="../search/search.do?word=${param.word }&pageNum=${endPage+1 }">[다음]</a>
		</c:if>
	</div>
	</div>
</div>
</body>
</html>
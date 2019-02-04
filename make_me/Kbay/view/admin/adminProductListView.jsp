<%@ page language="java" contentType="text/html; charset=UTF-8"
	pageEncoding="UTF-8"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<!DOCTYPE html>
<html>
<head>
<meta charset="UTF-8">
<title>경매 상품</title>
</head>
<body>
	<div id="content">
		<c:if test="${p_class_id2 == 'A1'}">
			<h3>Fashion > 여성의류</h3>
		</c:if>
		<c:if test="${p_class_id2 == 'A2' }">
			<h3>Fashion > 남성의류</h3>
		</c:if>
		<c:if test="${p_class_id2 == 'A3' }">
			<h3>Fashion > 액세서리</h3>
		</c:if>
		<c:if test="${p_class_id2 == 'B1' }">
			<h3>Electric > 스마트홈</h3>
		</c:if>
		<c:if test="${p_class_id2 == 'B2' }">
			<h3>Electric > 컴퓨터</h3>
		</c:if>
		<c:if test="${p_class_id2 == 'B3' }">
			<h3>Electric > 핸드폰</h3>
		</c:if>
		<c:if test="${p_class_id2 == 'C1' }">
			<h3>Sports > 피트니스</h3>
		</c:if>
		<c:if test="${p_class_id2 == 'C2' }">
			<h3>Sports > 캠핑용품</h3>
		</c:if>
		<c:if test="${p_class_id2 == 'D1' }">
			<h3>Health&Beauty > 코스매틱</h3>
		</c:if>
		<c:if test="${p_class_id2 == 'D2' }">
			<h3>Health&Beauty > 건강식품</h3>
		</c:if>
		<c:if test="${p_class_id2 == 'E1' }">
			<h3>Collection > 피규어</h3>
		</c:if>
		<c:if test="${p_class_id2 == 'E2' }">
			<h3>Collection > 빈티지</h3>
		</c:if>
	</div>
	<hr>
	<table>
		<tr>
			<th width="10%">번호</th>
			<th width="10%">상품명</th>
			<th width="10%">대분류</th>
			<th width="10%">소분류</th>
			<th width="10%">가격</th>
			<th width="10%">사용여부</th>
			<th width="10%">경매시작일</th>
			<th width="10%">경매마감일</th>
			<th width="10%">삭제</th>
		</tr>
		<c:if test="${empty list}">
			<tr>
				<th colspan="8">등록된 상품이 없습니다</th>
			</tr>
		</c:if>
		<c:if test="${not empty list }">
			<c:forEach var="product" items="${list }">
				<tr>
					<c:set var="total" value="${ total }"></c:set>
					<td>${ total }<c:set var="total" value="${ total-1 }"/></td>
					<td width="10%">${product.p_name }</td>
					<td width="10%">${product.p_class_name1 }</td>
					<td width="10%">${product.p_class_name2 }</td>
					<td width="10%">${product.p_price }</td>
					<td width="10%">${product.p_used }</td>
					<td width="10%">${product.p_start_date }</td>
					<td width="10%">${product.p_end_date }</td>
					<td width="10%"><a href="AdminProductDelete.do?num=${product.p_num }&class_id1=${product.p_class_id1 }&class_id2=${product.p_class_id2 }&pageNum=${currentPage}" onclick="return confirm('상품을 삭제하겠습니까?');">삭제</a></td>			
				</tr>
			</c:forEach>
		</c:if>
	</table>
	<div align="center">
		<c:if test="${startPage > pagePerBlock}">
			<a href="../admin/AdminProductListView.do?class_id1=${p_class_id1 }&class_id2=${p_class_id2 }&pageNum=${startPage - 1 }">[이전]</a>
		</c:if>		
		<c:forEach var="i" begin="${startPage }" end="${endPage }">
			<a href="../admin/AdminProductListView.do?class_id1=${p_class_id1 }&class_id2=${p_class_id2 }&pageNum=${i }">${i }</a>
		</c:forEach>
		<c:if test="${endPage < totPage }">
			<a href="../admin/AdminProductListView.do?class_id1=${p_class_id1 }&class_id2=${p_class_id2 }&pageNum=${endPage + 1 }">[다음]</a>
		</c:if>
	</div>
</body>
</html>
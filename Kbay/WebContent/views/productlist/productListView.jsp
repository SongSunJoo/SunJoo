<%@ page language="java" contentType="text/html; charset=UTF-8"
	pageEncoding="UTF-8"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
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
<style type="text/css">
	ul li {
		list-style: none;
	}
</style>
</head>
<body>
	<div class="container">
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
	<div class="row">
	
		<!-- md : pc 크기의 컬럼을 2개 차지 -->
		<div class="col-md-2">
			<div class="list-group">
			<c:if test="${p_class_id1 == 'A'}">
				<ul>
					<li><a	class="list-group-item"
						href="../productlist/ProductListView.do?class_id1=A&class_id2=A1">여성의류</a></li>
					<li><a	class="list-group-item"
						href="../productlist/ProductListView.do?class_id1=A&class_id2=A2">남성의류</a></li>
					<li><a	class="list-group-item"
						href="../productlist/ProductListView.do?class_id1=A&class_id2=A3">액세서리</a></li>
				</ul>
			</c:if>
			<c:if test="${p_class_id1 == 'B' }">
				<ul>
					<li><a	class="list-group-item"
						href="../productlist/ProductListView.do?class_id1=B&class_id2=B1">스마트홈</a></li>
					<li><a	class="list-group-item"
						href="../productlist/ProductListView.do?class_id1=B&class_id2=B2">컴퓨터</a></li>
					<li><a	class="list-group-item"
						href="../productlist/ProductListView.do?class_id1=B&class_id2=B3">핸드폰</a></li>
				</ul>
			</c:if>
			<c:if test="${p_class_id1 == 'C' }">
				<ul>
					<li><a	class="list-group-item"
						href="../productlist/ProductListView.do?class_id1=C&class_id2=C1">피트니스</a></li>
					<li><a	class="list-group-item"
						href="../productlist/ProductListView.do?class_id1=C&class_id2=C2">캠핑용품</a></li>
				</ul>
			</c:if>
			<c:if test="${p_class_id1 == 'D' }">
				<ul>
					<li><a	class="list-group-item"
						href="../productlist/ProductListView.do?class_id1=C&class_id2=D1">코스매틱</a></li>
					<li><a	class="list-group-item"
						href="../productlist/ProductListView.do?class_id1=C&class_id2=D2">건강식품</a></li>
				</ul>
			</c:if>
			<c:if test="${p_class_id1 == 'E' }">
				<ul>
					<li><a	class="list-group-item"
						href="../productlist/ProductListView.do?class_id1=E&class_id2=E1">피규어</a></li>
					<li><a	class="list-group-item"
						href="../productlist/ProductListView.do?class_id1=E&class_id2=E2">빈티지</a></li>
				</ul>
			</c:if>
			</div>
		</div>
		<div class="col-md-10 cont">
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
				<c:if test="${empty list}">
					<tr>
						<td colspan="8" align="center">등록된 상품이 없습니다</td>
					</tr>
				</c:if>
				<c:if test="${not empty list }">
					<c:forEach var="product" items="${list }">
						<tr>
							<c:set var="total" value="${ total }"></c:set>
							<td>${ total }<c:set var="total" value="${ total-1 }"/></td>
							<td><a href="../productlist/ProductListDetail.do?p_num=${product.p_num }&pageNum=${currentPage}">${product.p_name }</a></td>
							<%-- <td><a href="../product/productDetail.do?p_num=${product.p_num }&gubun=1&pageNum=${currentPage}">${product.p_name }</a></td> --%>
							<td>${product.p_class_name1 }</td>
							<td>${product.p_class_name2 }</td>
							<td>${product.p_price }</td>
							<td>${product.p_used }</td>
							<td>${product.p_start_date }</td>
							<td>${product.p_end_date }</td>
						</tr>
					</c:forEach>
				</c:if>
			</table>
			<div align="center">
				<c:if test="${startPage > pagePerBlock }">
					<a href="../productlist/ProductListView.do?class_id1=${p_class_id1 }&class_id2=${p_class_id2 }&pageNum=${startPage-1 }">[이전]</a>
				</c:if>		
				<c:forEach var="i" begin="${startPage }" end="${endPage }">
					<a href="../productlist/ProductListView.do?class_id1=${p_class_id1 }&class_id2=${p_class_id2 }&pageNum=${i }">${i }</a>
				</c:forEach>
				<c:if test="${endPage < totPage }">
					<a href="../productlist/ProductListView.do?class_id1=${p_class_id1 }&class_id2=${p_class_id2 }&pageNum=${endPage+1 }">[다음]</a>
				</c:if>
			</div>
		</div>
		</div>
	</div>

	</div>
</body>
</html>
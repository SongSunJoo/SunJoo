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
<table>
	<tr>
		<th width="10%">번호</th>
		<th width="10%">제목</th>
		<th width="10%">작성자</th>
		<th width="10%">등록일</th>
		<th width="10%">수정</th>
		<th width="10%">삭제</th>
	</tr>
	<c:if test="${empty list }">
	<tr>
		<td colspan="4">등록된 공지사항이 없습니다.</td>
	</tr>
	</c:if>
	<c:if test="${not empty list }">
		<c:forEach var="knotice" items="${list }">
			<tr>
				<c:set var="total" value="${total }"></c:set>
				<td>${total }<c:set var="total" value="${total-1 }" /></td>
				<td><a href="AdminKnoticeView.do?n_num=${knotice.n_num }&pageNum=${currentPage}">${knotice.n_subject }</a></td>
				<td>${knotice.m_id }</td>
				<td>${knotice.n_reg_date }</td>
				<td><a href="AdminKnoticeUpdateForm.do?n_num=${knotice.n_num }&pageNum=${currentPage}">수정</a></td>
				<td><a href="AdminKnoticeDelete.do?n_num=${knotice.n_num }&pageNum=${currentPage}" onclick="return confirm('해당 공지사항을 삭제하시겠습니까?');">삭제</a></td>
			</tr>
		</c:forEach>		
	</c:if>
</table>
<div align="center">
	<c:if test="${startPage > pagePerBlock }">
		<a href="AdminKnoticeList.do?pageNum=${startPage - 1 }">[이전]</a>
	</c:if>
	<c:forEach var="i" begin="${startPage }" end="${endPage }">
		<a href="AdminKnoticeList.do?pageNum=${i }">${i }</a>
	</c:forEach>
	<c:if test="${endPage < totPage }">
		<a href="AdminKnoticeList.do?pageNum=${endPage + 1 }">[다음]</a>
	</c:if>
</div>
<a href="AdminKnoticeInsertForm.do">등록</a>
</body>
</html>
<%@ page language="java" contentType="text/html; charset=UTF-8"
    pageEncoding="UTF-8"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<!DOCTYPE html>
<html>
<head>
<meta charset="UTF-8">
<meta http-equiv="X-UA-Compatible" content="IE=edge">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Insert title here</title>
<link href="../../css/bootstrap.min.css" rel="stylesheet">
<script type="text/javascript" src="../../js/jquery.js"></script>
<script src="../../js/bootstrap.min.js"></script>
</head>
<body>
<div class="container">
	<div id="content" align="center">
		<div class="row">
			<div class="col-md-10">
				<h3>게시글 목록</h3>
			</div>
			<div class="col-md-2">
				<a href="B_insertForm.do?pageNum=1" class="btn btn-default" role="button">게시글 입력 </a>
			</div>
		</div>
		<table class="table table-hover">
			<tr><th>번호</th><th>제목</th><th>작성자</th>
				<th>조회수</th><th>작성일</th></tr>	
			<!-- 만약에 게시글이 없을 때 -->
			<c:if test="${empty list}">
				<tr><th colspan="5">게시글이 없습니다</th></tr>
			</c:if>
			
			<!-- 만약에 list가 empty가 아닐때, 반복문 실행 -->
			<c:if test="${not empty list }">
				<!-- list와 total(전체 갯수) 불러옴 -->
				<c:forEach var="kboard" items="${list}">
					<c:set var="total" value="${total}"/>
						<tr><td>${total}
						<c:set var="total" value="${total-1}"/></td>
						<td title="${kboard.b_subject}">
							<!-- 댓글 -->
							<c:if test="${kboard.b_re_level > 0 }">
							<img alt="" src="images/level.gif" height="5" 
								width="${kboard.b_re_level*10}">
							<img alt="" src="images/re.gif"></c:if>
							<a href="B_view.do?b_num=${kboard.b_num}&pageNum=${currentPage}">
								${kboard.b_subject}</a>
							<!-- 인기글  -->
							<c:if test="${kboard.b_readcount > 10 }">
								<img alt="" src="images/hot.gif"></c:if></td>
						<td>${kboard.id}</td>
						<td>${kboard.b_readcount}</td>
						<td>${kboard.b_reg_date}</td></tr>
				</c:forEach>
			</c:if>
		</table>
		<div align="center">
			<!-- 만약 시작 페이지가 페이지당 블럭보다 크면 -->
			<c:if test="${startPage > pagePerBlock }">
				<!-- list.do로 가되, 시작 페이지보다 1페이지 전의 페이지 넘버를 가지고 감 -->
				<a href="list.do?pageNum=${startPage-1}">[이전]</a>
			</c:if>
			<c:forEach var="i" begin="${startPage}" end="${endPage }">
				<a href="list.do?pageNum=${i}">[${i}]</a>
			</c:forEach>
			<c:if test="${endPage < totPage }">
				<a href="list.do?pageNum=${endPage+1}">[다음]</a>
			</c:if>
		</div>
	</div>
</div>
</body>
</html>
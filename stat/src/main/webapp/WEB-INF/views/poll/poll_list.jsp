<%@ page language="java" contentType="text/html; charset=UTF-8"
    pageEncoding="UTF-8"%>
<%@ include file="../header.jsp" %>
<!DOCTYPE html>
<html>
<head>
<meta charset="UTF-8">
<title>DataCup2019</title>
<script type="text/javascript">
	function plist(senddata){
		$.post("poll_list.do",senddata, function (data) {			
			$('#poll_list').html(data);
		})
	}
</script>
</head>
<body>
	<div class="poll_list">
		<div class="poll_board_title">
			<font size="3">[ 종료된 데이터 월드컵 목록 ]</font>
		</div>
		<div class="poll_board_plus" align="right">
			<a href="poll_insertForm.do"> + 설문조사 등록하기</a></div><br>
		<div class="poll_board">
			<table class="table table-hover">
				<tr><th>번호</th><th>질문 제목</th><th>선택1</th><th>선택2</th><th>시작일</th><th>종료일</th></tr>	
					<!-- 만약에 게시글이 없을 때 -->
					<c:if test="${empty list}">
						<tr><th colspan="6">게시글이 없습니다</th></tr>
					</c:if>
					
					<!-- 만약에 list가 empty가 아닐때, 반복문 실행 -->
					<c:if test="${not empty list}">
						<!-- list와 total(전체 갯수) 불러옴 -->
						<c:forEach var="poll" items="${list}">
							<tr><td>${poll.poll_id}</td>
								<td><a href="poll_result.do?poll_id=${poll.poll_id}&pageNum=${pb.currentPage}">
										${poll.poll_title}</a></td>
								<td>${poll.poll_cont1}</td>
								<td>${poll.poll_cont2}</td>
								<td>${poll.poll_startDate}</td>
								<td>${poll.poll_endDate}</td></tr>
						</c:forEach>
					</c:if>
				</table>
				<div align="center">
					<input type="hidden" name="poll_id" value="${poll.poll_id}">
					<ul class="pagination">
						<c:if test="${pb.startPage > pb.pagePerBlock}">
							<li><button onclick="poll_list.do?pageNum=1&poll_id=${poll.poll_id}"><span class="glyphicon glyphicon-backward"></span></button></li>
							<li><button onclick="poll_list.do?pageNum=${pb.startPage - 1}&poll_id=${poll.poll_id}" class="glyphicon glyphicon-triangle-left"></button></li>
						</c:if>
						<c:forEach var="i" begin="${pb.startPage}" end="${pb.endPage}">
							<c:if test="${i==pb.currentPage}">
								<li class="active"><button onclick="poll_list.do?pageNum=${i}&poll_id=${poll.poll_id}">${i}</button></li>
							</c:if>
							<c:if test="${i!=pb.currentPage}">
								<li><button onclick="plist('pageNum=${i}&poll_id=${poll.poll_id}')">${i}</button></li>
							</c:if>
						</c:forEach>
						<c:if test="${pb.endPage < pb.totalPage}">
							<li><button onclick="poll_list.do?pageNum=${pb.endPage + 1}&poll_id=${poll.poll_id}" class="glyphicon glyphicon-triangle-right"></button></li>
							<li><button onclick="poll_list.do?pageNum=${pb.totalPage}&poll_id=${poll.poll_id}"><span class="glyphicon glyphicon-forward"></span></button></li>
						</c:if>
					</ul>
				</div>
	</div>
</div>
</body>
</html>
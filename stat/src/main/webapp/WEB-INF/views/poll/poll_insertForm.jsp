<%@ page language="java" contentType="text/html; charset=UTF-8"
    pageEncoding="UTF-8"%>
<%@ include file="../header.jsp" %>
<%@ page import="java.text.SimpleDateFormat" %>
<!DOCTYPE html>
<html>
<head>
<meta charset="UTF-8">
<%	Date date = new Date();
	Date max_date = new Date();
	max_date.setDate(max_date.getDate()+7);
	SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd");
	System.out.println(sdf);
%>
<c:set var="date" value="<%=sdf.format(date) %>"/>
<c:set var="max_date" value="<%=sdf.format(max_date) %>"/>

<title>Insert title here</title>
<script type="text/javascript">
	function chk1() {
		if (chk.poll_startDate.value >= chk.poll_endDate.value) {
			alert("설문 마감일은 설문 시작일 이후 날짜를 선택해주세요");
			chk.poll_endDate.value = "";
			return false;
		}
		var conf = confirm("입력한 내용은 수정 및 삭제가 불가합니다\n생성하시겠습니까?");
		if(conf){	return true;		}
		else {		return false;		}
 	}
	
</script>
</head>
<body>
<div class="container">
	<div class="header">
		<h3 style="font-style: italic; font-size:22px; font-family: cursive; font-weight: bold; color: #FFBB00;">DataCup2019</h3>
		<!-- <h3 class="text-primary">DataCup 2019</h3> -->
	</div>
	
	<div class="uppermain" align="center">
		<div class="row">
			<div class="title" align="center" style="padding: 15px 15px;">
				<font size="4" style="font-weight:bold;"> · 데이터 월드컵 만들기</font>
			</div>
			<div class="col-md-2"></div>
			<div class="col-md-8">
				<form action="poll_insert.do" method="post" name="chk" id="chk" onsubmit="return chk1()">
					<table border="0" class="table table-bordered" width="60%">
						<tr><td width="20%">질문 제목</td>
							<td width="80%"><input type="text" name="poll_title" id="poll_title" required="required" autofocus="autofocus"></td></tr>
						<tr><td>질문 내용 1</td>
							<td><input type="text" name="poll_cont1" id="poll_cont1" required="required"></td></tr>
						<tr><td>질문 내용 2</td>
							<td><input type="text" name="poll_cont2" id="poll_cont2" required="required"></td></tr>
						<tr><td>설문 시작일</td>
							<td><input type="date" class="form-control" name="poll_startDate" id="poll_startDate" min="${date}" value="${date}"
							readonly="readonly"></td></tr>
						<tr><td>설문 마감일</td>
							<td><input type="date" class="form-control" name="poll_endDate" id="poll_endDate" max="${max_date}"></td></tr>
						<tr><td colspan="2">※ 설문 기간은 설문 시작일 기준 최대 7일까지만 가능합니다.<br>
											※ 한번 등록 된 게시글은 수정 및 삭제가 불가능합니다. <br> 입력한 내용을 확인해주세요.</td></tr>						
					</table>
					<div class="row" align="center">
						<input type="submit" class="btn btn-default" value="확인">
						<input type="button" class="btn btn-default" value="뒤로가기" onclick="history.go(-1)">
					</div>
				</form>
			</div>
			<div class="col-md-2"></div>
	</div>
</div>
</body>
</html>
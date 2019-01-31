<%@ page language="java" contentType="text/html; charset=UTF-8"
    pageEncoding="UTF-8"%>
<%@ include file="header.jsp" %>
<!DOCTYPE html>
<html>
<head>
<meta charset="UTF-8">
<title>Insert title here</title>
<style type="text/css">
<link href="https://fonts.googleapis.com/css?family=Nanum+Gothic" rel="stylesheet">
	body {
		background-color: #C2E2E8;
		font-color: white;
		font-family: 'Nanum Gothic', sans-serif;
	}
	table, td, th {
		padding: 10px 10px;
	}
</style>
</head>
<body>
	<div class="main_poll">
		<div class="row1" style="padding:10px 10px;" align="center"><h3><b>우리가 만드는 데이터 월드컵</b></h3></div>
		<div class="row2">
			<table align="center">
			<tr><td colspan="3">
				<font size="3" color="white">Q. {탕수육의 진리는?}</font></td></tr>
			<tr><td width="150">{답변1 출력}</td>
				<td width="100"><font size="5" color="#ED5560"><b>VS</b></font></td>
				<td width="150">{답변2 출력}</td></tr></table>
		</div>
		<div class="row3" align="center" style="padding:30px 30px;">
			<input type="submit" class="btn btn-warning" value="투표하기"/>　　
			<input type="submit" class="btn btn-inverse" value="결과보기"/>
		</div>
	</div>
</body>
</html>
<%@ page language="java" contentType="text/html; charset=UTF-8"
    pageEncoding="UTF-8"%>
<%@ include file="../header.jsp" %>
<!DOCTYPE html>
<html>
<head>
<meta charset="UTF-8">
<title>DataCup2019</title>
<script type="text/javascript">
	$(function() {
		$('#poll_list').load('poll_list.do?pageNum=${pageNum}');
	});
</script>
</head>
<body>
<div class="container">
	<div class="header">
		<a href="survey1_main.do">
		<h3 style="font-style: italic; font-size:22px; font-family: cursive; font-weight: bold; color: #FFBB00;">
		DataCup2019</h3></a>
	</div>
	
	<div class="uppermain" align="center">
		<div class="row">
			
			<div class="title" align="center" style="padding: 15px 15px;">
				<font size="4" style="font-weight:bold;"> · 우리가 만드는 데이터 월드컵 결과보기</font>
			</div>

			<div class="col-md-1"></div>
			<div class="col-md-10">
				<input type="hidden" name="poll_id" value="${poll.poll_id}">
				<table border="0" style="background-color: #B3CFD4;">
					<tr height="40">
						<td colspan="3" ><font size="4" style="font-weight:bold;">&nbsp;&nbsp;Q. ${poll_result.poll_title}</font></td>
					</tr>
					<tr height="100">
						<td width="350" align="center">${poll_result.poll_cont1}</td>
						<td width="80" align="center"><font size="6" color="#D9534F" style="font-weight:bold;">VS</font></td>
						<td width="350" align="center">${poll_result.poll_cont2}</td>
					</tr>
					<tr height="10">
						<td colspan="3" align="center" width="80%">
							<div class="progress">
								<div class="progress-bar progress-bar-success" style="width: ${poll_cont1_per}%">
									<span class="sr-only"> ${poll_cont1_per}%</span>
								</div>
								<div class="progress-bar progress-bar-warning" style="width: ${poll_cont2_per}%">
									 <span class="sr-only">${poll_cont2_per}%</span>
								</div>
							</div>
						</td>
					</tr>
					<tr height="50">
						<td align="center">득표수 ${plid1}, 득표율 ${poll_cont1_per}%</td>
						<td></td>
						<td align="center">득표수 ${plid2}, 득표율 ${poll_cont2_per}%</td>
					</tr>
				</table><br><br>
				<div id="poll_list"></div>
			</div>
			
			<div class="col-md-1"></div>
		</div>
	</div>
</div>
<div class="container">
	<div align="center">
		서울 강남구 테헤란로 7길  DataCup Tel. 02-1234-5678 <br> 
		Copyright ⓒ 2019 by DataCup, Designed by DataCup
	</div>
</div>	
	
</body>
</html>
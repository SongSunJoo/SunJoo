<%@ page language="java" contentType="text/html; charset=UTF-8"
    pageEncoding="UTF-8"%>
<%@ include file="../header.jsp" %>
<!DOCTYPE html>
<html>
<head>
<meta charset="UTF-8">
<title>DataCup2019</title>

</head>
<body>
<div class="container">
	<div class="header">
		<h3 style="font-style: italic; font-size:22px; font-family: cursive; font-weight: bold; color: #FFBB00;">DataCup2019</h3>
	</div>
	
	<div class="uppermain" align="center">
		<div class="row">
			<div class="col-md-3"></div>
			<div class="col-md-6">
				<form action="survey11R_allYou.do">
					<input type="hidden" name="user_id" value="${user_id }">
					<br>
					<h4 style="color: white; font-weight: bold;">닉네임을 입력해 주세요</h4>
					<hr>
					<div class="main">
						<br>
						<table>
							<tr><td>
								<input type="text" name="user_nick" id="user_nick" required="required" align="center"
								style="border:0; background:#EAEAEA; font-size: 50; font-weight: bold; width: 300px;">
							</td></tr>
						</table>
					</div>
					<div class="row">
						<br><br>
						설문 결과를 바탕으로 당신에 대한 리포트를 생성합니다<br><br><br><br>
						<input type="submit" class="btn btn-danger" value="　  　다  음　▶　  " style="font-size: 16; font-color: #000000; font-weight: bold;"/>
						<br><br>
				</form>
			</div>
			<div class="col-md-3"></div>
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
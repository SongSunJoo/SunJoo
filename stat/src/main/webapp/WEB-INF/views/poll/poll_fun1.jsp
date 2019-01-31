<%@ page language="java" contentType="text/html; charset=UTF-8"
    pageEncoding="UTF-8"%>
<%@ include file="../header.jsp" %>
<!DOCTYPE html>
<html>
<head>
<meta charset="UTF-8">
<title>Insert title here</title>
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
				<font size="4" style="font-weight:bold;"> · 2045년 우리는?</font>
			</div>
			<div class="col-md-2"></div>
			<div class="col-md-8">
				<img alt="" src="${path}/images/2045futurepoll.jpg" height="500px">
					<div class="row" align="center"><br>
						<input type="button" class="btn btn-default" value="뒤로가기" onclick="history.go(-1)">
						<div>&nbsp;</div>
					</div>
			</div>
			<div class="col-md-2"></div>
	</div>
</div>
</body>
</html>
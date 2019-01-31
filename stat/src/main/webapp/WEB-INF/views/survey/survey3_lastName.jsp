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
<form action="survey3R_lastName.do">
	<input type="hidden" name="user_id" value="${user_id }">
	<div class="header">
		<%-- ${user_id } --%>
		<h3 style="font-style: italic; font-size:22px; font-family: cursive; font-weight: bold; color: #FFBB00;">DataCup2019</h3>
	</div>
	<div>
		<div class="row">
			<div class="col-md-4"></div>
			<div class="col-md-4">
			</div>
			<div class="col-md-4"></div>
		</div>
	</div>
	<div class="uppermain" style="width: 100%" >
		<div class="row">
			<div class="col-md-3"></div>
			<div class="col-md-6" align="center">
				<br>
				<h4 style="color: white; font-weight: bold;">당신의 성씨를 입력하세요</h4>
				<hr>
				<div class="input-group">
					<span class="input-group-addon">
						<span class="glyphicon glyphicon-user"></span>
					</span>
					<input type="text" name="lastName" class="form-control" placeholder="성씨를 입력하세요" required="required" autofocus="autofocus">
				</div><br>
				<h5 style="color: white;">당신과 같은 성씨의 사람들이 대한민국에 얼마나 있는지 알려드립니다</h5>
				<br>
				<input type="submit" class="btn btn-danger" value="　  　다  음　▶　  " style="font-size: 16; font-color: #000000; font-weight: bold;"/>
				<div>&nbsp;</div>
			</div>
			<div class="col-md-3"></div>
		</div>
	</div>
</form>
</div>				
</body>
</html>
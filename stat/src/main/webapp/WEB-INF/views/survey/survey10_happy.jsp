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
<form action="survey10R_happy.do">
	<input type="hidden" name="user_id" value="${user_id }">
	<div class="header">
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
				<h4 style="color: white; font-weight: bold;">당신은 평소에 얼마나 행복하다고 느끼나요?</h4>
				<hr>
				<img alt="" src="${path }/images/happy.png" width="100px">
				<div>&nbsp;</div>
				<div class="btn-group" data-toggle="buttons" align="left"><p>
					<label id="old" class="btn btn-primary">
						<input type="radio" name="happyDB_id" value=1 checked="checked">아주 행복하다</label><p>
					<label id="old" class="btn btn-primary">
						<input type="radio" name="happyDB_id" value=2 >대체로 행복하다</label><p>
					<label id="old" class="btn btn-primary">
						<input type="radio" name="happyDB_id" value=3 >보통이다</label><p>
					<label id="old" class="btn btn-primary">
						<input type="radio" name="happyDB_id" value=4 >대체로 불행하다</label><p>
					<label id="old" class="btn btn-primary">
						<input type="radio" name="happyDB_id" value=5 >아주 불행하다</label><p>
				</div>
				<br><br>
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
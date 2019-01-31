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
<form action="survey7R_digital.do">
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
				<h4 style="color: white; font-weight: bold;">당신은 여가시간에 주로 무엇을 하나요?</h4>
				<hr>
				<img alt="" src="${path }/images/digital.png" width="100px">
				<div>&nbsp;</div>
				<div class="btn-group" data-toggle="buttons" align="left"><p>
					<label id="old" class="btn btn-primary">
						<input type="radio" name="digitalDB_id" value=1 checked="checked">스마트폰을 한다</label>
					<label id="old" class="btn btn-primary">
						<input type="radio" name="digitalDB_id" value=2 >TV를 본다</label><p>
					<label id="old" class="btn btn-primary">
						<input type="radio" name="digitalDB_id" value=3 >컴퓨터를 한다&nbsp;&nbsp;&nbsp;&nbsp;</label>
					<label id="old" class="btn btn-primary">
						<input type="radio" name="digitalDB_id" value=4 >게임을 한다</label><p>
					<label id="old" class="btn btn-primary">
						<input type="radio" name="digitalDB_id" value=5 >영화를 본다&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;</label>
					<label id="old" class="btn btn-primary">
						<input type="radio" name="digitalDB_id" value=6 >집(방)을 청소한다</label><p>
					<label id="old" class="btn btn-primary">
						<input type="radio" name="digitalDB_id" value=7 >운동을 한다&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;</label>
					<label id="old" class="btn btn-primary">
						<input type="radio" name="digitalDB_id" value=8 >친구나 지인을 만난다</label><p>
					<label id="old" class="btn btn-primary">
						<input type="radio" name="digitalDB_id" value=9 >(낮)잠을 잔다&nbsp;&nbsp;&nbsp;&nbsp;</label>
					<label id="old" class="btn btn-primary">
						<input type="radio" name="digitalDB_id" value=10 >책을 읽는다</label>
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
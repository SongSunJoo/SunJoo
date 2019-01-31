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
<form action="survey6R_old.do">
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
				<h4 style="color: white; font-weight: bold;">당신이 늙었다고 생각될 때는?</h4>
				<hr>
				<img alt="" src="${path }/images/old.png" width="150px">
				<div>&nbsp;</div>
				<div class="btn-group" data-toggle="buttons" align="left"><p>
					<label id="old" class="btn btn-primary">
						<input type="radio" name="oldDB_id" value=1 checked="checked">늦게까지 노는게 피곤하고 다음날이 걱정될 때</label><p>
					<label id="old" class="btn btn-primary">
						<input type="radio" name="oldDB_id" value=2 >교복입은 학생들이 예뻐 보일 때</label><p>
					<label id="old" class="btn btn-primary">
						<input type="radio" name="oldDB_id" value=3 >주변 사람들이 하나 둘 결혼할 때</label><p>
					<label id="old" class="btn btn-primary">
						<input type="radio" name="oldDB_id" value=4 >새롭게 도전하는 것이 망설여질 때</label><p>
					<label id="old" class="btn btn-primary">
						<input type="radio" name="oldDB_id" value=5 >군인을 아저씨라고 부르기 민망할 때</label><p>
					<label id="old" class="btn btn-primary">
						<input type="radio" name="oldDB_id" value=6 >건강식품, 건강 프로그램을 찾게 될 때</label><p>
					<label id="old" class="btn btn-primary">
						<input type="radio" name="oldDB_id" value=7 >요즘 개그가 이해가 안될 때</label><p>
					<label id="old" class="btn btn-primary">
						<input type="radio" name="oldDB_id" value=8 >낯선 사람이랑 얘기하는게 어색하지 않을 때</label><p>
					<label id="old" class="btn btn-primary">
						<input type="radio" name="oldDB_id" value=9 >물건값을 깎는 것에 집착할 때</label>
				</div>
				<br>	
				<!-- <div>&nbsp;</div> -->
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
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
<form action="survey9R_trans.do">
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
				<h4 style="color: white; font-weight: bold;">당신이 부자가 될 확률은?</h4>
				<hr>
				<img alt="" src="${path }/images/rich.png" width="100px">
				<div>&nbsp;</div>
				<div class="btn-group" data-toggle="buttons" align="left"><p>
					<label id="old" class="btn btn-primary">
						<input type="radio" name="transDB_id" value=1 checked="checked">확실하다. 난 날아오를테다</label><p>
					<label id="old" class="btn btn-primary">
						<input type="radio" name="transDB_id" value=2 >날진 못해도 가능성은 있다</label><p>
					<label id="old" class="btn btn-primary">
						<input type="radio" name="transDB_id" value=3 >그냥 그렇다</label><p>
					<label id="old" class="btn btn-primary">
						<input type="radio" name="transDB_id" value=4 >다음 생을 기약한다</label><p>
					<label id="old" class="btn btn-primary">
						<input type="radio" name="transDB_id" value=5 >모르겠다</label><p>
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
<%@ page language="java" contentType="text/html; charset=UTF-8"
    pageEncoding="UTF-8"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@ include file="../header.jsp" %>
<!DOCTYPE html>
<html>
<head>
<meta charset="UTF-8">
<title>DataCup2019</title>

</head>
<body>
<div class="container">
<form action="survey3_lastName.do">
	<input type="hidden" name="user_id" value="${user_id }">
	<div class="header">
		<h3 style="font-style: italic; font-size:22px; font-family: cursive; font-weight: bold; color: #FFBB00;">DataCup2019</h3>
		<!-- <h3 class="text-primary">DataCup 2019</h3> -->
	</div>
	<div>
		<div class="row">
			<div class="col-md-4"></div>
			<div class="col-md-4">
				<div class="navigation" align="center">
					<img alt="" src="${path }/images/circle_white.png">
					<img alt="" src="${path }/images/circle.png">
					<img alt="" src="${path }/images/circle_white.png">
					<img alt="" src="${path }/images/circle_white.png">
					<img alt="" src="${path }/images/circle_white.png">
					<img alt="" src="${path }/images/circle_white.png">
					<img alt="" src="${path }/images/circle_white.png">
					<img alt="" src="${path }/images/circle_white.png">
					<img alt="" src="${path }/images/circle_white.png">
					<img alt="" src="${path }/images/circle_white.png">
					
				</div>
			</div>
			<div class="col-md-4"></div>
		</div>
	</div>
	<div class="uppermain" align="center">
		<div class="row">
			<div class="col-md-3"></div>
			<div class="col-md-6">
					<div>
						<br>
						<section class="box1 pull-center" style="font-size:22px; font-family: serif; font-weight: bold; color: white;">당신은 ${height_value} cm 키의 소유자로군요!</section>
						<hr>
						<h4>설문조사 응답자들의 키의 분포는 다음과 같습니다 </h4><br>
						<div class="row">
							<div class="col-md-7"></div>
							<div class="col-md-5">
							(단위 : cm)
							</div>
						</div>
						<img alt="" src="${path }/fileSave/HeightChart.png" width="400px">
						</div>
					<br><br>
					<div class="row">
							<h5 align="left">대한민국 <b>남자</b> 평균 키는 <b>173.5cm</b>입니다 </h5>
					      	<div class="progress">
					      		
					      		<!-- <span class="glyphicon glyphicon-king" aria-hidden="true"></span> -->
								<div class="progress-bar" role="progressbar" 
									aria-valuenow="174" aria-valuemin="0" aria-valuemax="200"style=" width: 86.75%;">
								<span class="sr-only"></span></div>
							</div>
							 <!-- <span class="glyphicon glyphicon-king" aria-hidden="true"></span>　　대한민국 남자 평균 키는 173.5cm입니다.<br><br> -->
					        <br>          
					        <h5 align="left">대한민국 <b>여자</b> 평균 키는 <b>161.1cm</b>입니다</h5> 
					      	<div class="progress">
								<div class="progress-bar progress-bar-warning" role="progressbar" 
									aria-valuenow="161" aria-valuemin="0" aria-valuemax="200" style="width: 80.55%;">
								<span class="sr-only"></span></div>
							</div>
							 <!-- <span class="glyphicon glyphicon-queen" aria-hidden="true"></span> --><!-- 　　대한민국 여자 평균 키는 161.1cm입니다. --><!-- <br><hr> -->
						
						<br>
						<input type="submit" class="btn btn-danger" value="　  　다  음　▶　  " style="font-size: 16; font-color: #000000; font-weight: bold;"/>
						<div>&nbsp;</div>
						<br>
					</div>
			</div>
			<div class="col-md-3"></div>
		</div>
	</div>
</form>	
<!-- </div>
<div class="container"> -->
	<div align="center">
		서울 강남구 테헤란로 7길  DataCup Tel. 02-1234-5678 <br> 
		Copyright ⓒ 2019 by DataCup, Designed by DataCup
	</div>
</div>	
	
</body>
</html>
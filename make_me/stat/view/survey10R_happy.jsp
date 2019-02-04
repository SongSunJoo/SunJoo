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
<form action="survey11_nick.do">
	<input type="hidden" name="user_id" value="${user_id }">
	<div class="header">
		<h3 style="font-style: italic; font-size:22px; font-family: cursive; font-weight: bold; color: #FFBB00;">DataCup2019</h3>
	</div>
	<div>
		<div class="row">
			<div class="col-md-4"></div>
			<div class="col-md-4">
				<div class="navigation" align="center">
					<img alt="" src="${path }/images/circle_white.png">
					<img alt="" src="${path }/images/circle_white.png">
					<img alt="" src="${path }/images/circle_white.png">
					<img alt="" src="${path }/images/circle_white.png">
					<img alt="" src="${path }/images/circle_white.png">
					<img alt="" src="${path }/images/circle_white.png">
					<img alt="" src="${path }/images/circle_white.png">
					<img alt="" src="${path }/images/circle_white.png">
					<img alt="" src="${path }/images/circle_white.png">
					<img alt="" src="${path }/images/circle.png">
					
				</div>
			</div>
			<div class="col-md-4"></div>
		</div>
	</div>
	<div class="uppermain" style="width: 100%" >
		<div class="row">
			<div class="col-md-3"></div>
			<div class="col-md-6" align="center">
				<h3 style="color: white;">당신이 평소에 느끼는 행복의 정도는 </h3>
				<h3 style="color: navy; font-weight: bold;">${happy_value }</h3>
				<h4>고 생각하시는군요 </h4>
				<hr>
				<h4>설문조사 응답자들이 느끼는 행복의 정도는 다음과 같습니다 </h4><br>
				<img alt="" src="${path }/fileSave/HappyChart.png" width="400px">
				<br>
				<hr>
				<h5>※ 다음은 2017년도 20세 이상 성인을 대상으로 조사한 결과입니다</h5>
				<img alt="" src="${path }/images/happy_data.jpg" width="350px">
				<div>&nbsp;</div>
				<input type="submit" class="btn btn-danger" value="　  　다  음　▶　  " style="font-size: 16; font-color: #000000; font-weight: bold;"/>
				<div>&nbsp;</div>
			<div class="col-md-3"></div>
		</div>
	</div>
</form>	
</div>
</body>
</html>
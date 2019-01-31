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
<form action="survey2_height.do">
	<input type="hidden" name="user_id" value="${user_id }">
	<div class="header">
		<h3 style="font-style: italic; font-size:22px; font-family: cursive; font-weight: bold; color: #FFBB00;">DataCup2019</h3>
	</div>
	<div>
		<div class="row">
			<div class="col-md-4"></div>
			<div class="col-md-4">
				<div class="navigation" align="center">
					<img alt="" src="${path }/images/circle.png">
					<img alt="" src="${path }/images/circle_white.png">
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
	<div class="uppermain">
		<div class="row">
			<div class="col-md-3"></div>
			<div class="col-md-6" align="center">
				<h3 style="color: white; font-weight: bold;">당신의 기대여명</h3>
				<hr>
				<c:if test="${result > 0 }">
					<c:set var="per1" value="${age/(age+rest_life)*100}"></c:set>
					<c:set var="per2" value="${rest_life/(age+rest_life)*100}"></c:set>
					<h4> 현재 만${age }세인 당신에게 남은 시간은</h4><br>
					<!-- <div class="jumbotron" style="height: 30%;"> -->
						<h1 style="font-weight: bold; font-size: 50px;"> ${rest_life }</h1>
					<!-- </div> -->
					<h4>년입니다.</h4><br>
					<div class="progress">
						<div class="progress-bar progress-bar-warning" style="width: ${per1}%;">
							<span class="sr-only">"${per1}"% Complete</span>
						</div>
						<%-- <div class="progress-bar" role="progressbar" style="width: ${per2}%" aria-valuemin="0" aria-valuemax="${per2/100}">
							<span class="sr-only">"${per2}"% Complete</span>
						</div> --%>
						<div class="progress-bar progress-bar-success" style="width: ${per2}%">
							<span class="sr-only">"${per2}"% Complete</span>
						</div>
					</div>
					<h3 style="color: white;">오늘을 마지막 같이 소중히 보내세요.</h3>
				</c:if>
				<br>
				<input type="submit" class="btn btn-danger" value="　  　다  음　▶　  " style="font-size: 16; font-color: #000000; font-weight: bold;"/>
				<div>&nbsp;</div>
			<div class="col-md-3"></div>
		</div>
	</div>
</form>	
</div>	
</body>
</html>
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
<form action="survey7_digital.do">
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
					<img alt="" src="${path }/images/circle.png">
					<img alt="" src="${path }/images/circle_white.png">
					<img alt="" src="${path }/images/circle_white.png">
					<img alt="" src="${path }/images/circle_white.png">
					<img alt="" src="${path }/images/circle_white.png">
					
				</div>
			</div>
			<div class="col-md-4"></div>
		</div>
	</div>
	<div>&nbsp;</div>
	<div class="uppermain" style="width: 100%" >
		<div class="row">
			<!-- <div class="col-md-1"></div> -->
			<div class="col-md-12" align="center">
				<h3 style="color: white;">당신이 가장 나이들었다고 느낄 때는 바로</h3>
				<h3 style="color: navy; font-weight: bold;">${old_value }</h3>
				<hr>
				<h4>설문조사 응답자들의 결과는 다음과 같습니다 </h4>
				<img alt="" src="${path }/fileSave/OldChart.png" width="600px" height="500px">
				<br>
				<hr>
				<h5>※ 다음은 연령대별 통계자료입니다</h5>
				<div>
					<img alt="" src="${path }/images/old_20.png" width="350px">
					<img alt="" src="${path }/images/old_30.png" width="350px">
					<img alt="" src="${path }/images/old_40.png" width="350px">
				</div>
				<div>&nbsp;</div>
				<input type="submit" class="btn btn-danger" value="　  　다  음　▶　  " style="font-size: 16; font-color: #000000; font-weight: bold;"/>
				<div>&nbsp;</div>
		</div>
	</div>
</form>	
</div>
</body>
</html>
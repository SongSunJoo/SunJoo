<%@ page language="java" contentType="text/html; charset=UTF-8"
    pageEncoding="UTF-8"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@ taglib prefix="fmt" uri="http://java.sun.com/jsp/jstl/fmt" %>
<%@ include file="../header.jsp" %>
<!DOCTYPE html>
<html>
<head>
<meta charset="UTF-8">
<title>DataCup2019</title>
</head>
<body>
<div class="container">
<form action="survey12_happyRel.do">
	<input type="hidden" name="user_id" value="${user_id }">
	<div class="header">
		<h3 style="font-style: italic; font-size:22px; font-family: cursive; font-weight: bold; color: #FFBB00;">DataCup2019</h3>
	</div>
	
	<div class="uppermain" align="center">
		<br>
		<div>
			<section class="box1 pull-center" style="font-size:22px; font-family: serif; font-weight: bold; color: white;">
						All about you, ${user_nick}</section>
			<hr><br>
		</div>
		<div>
			<table class="table table-bordered" style="background: #EAEAEA">
				<tr><td><b><em>${su.user_place}</em></b>에 살고 있는 <b><em>${group_value}</em></b> 그룹인 <b><em>${user_nick}</em></b>님은,<br>
						현재 만<b><em>${age_value}</em></b>살이며 남은 기대 여명은 <b><em>${rest_life}</em></b>년 입니다.<br>
						<br>
						<h5 align="left">대한민국 남자 평균수명은 80세입니다 </h5>
				      	<div class="progress">
							<div class="progress-bar" role="progressbar" 
								aria-valuenow="80" aria-valuemin="0" aria-valuemax="100"style=" width: 80%;">
							<span class="sr-only"></span></div>
						</div>          
				        <h5 align="left">대한민국 여자 평균수명은 86세입니다</h5> 
				      	<div class="progress">
							<div class="progress-bar progress-bar-warning" role="progressbar" 
								aria-valuenow="86" aria-valuemin="0" aria-valuemax="100" style="width: 86%;">
							<span class="sr-only"></span></div>
						</div>
					</td>
					<td>
						당신의 키는 <b><em>${su.height_value}</em></b>cm 입니다.<br><br><br>
						<h5 align="left">대한민국 남자 평균키는 173.5cm 입니다 </h5>
				      	<div class="progress">
							<div class="progress-bar" role="progressbar" 
								aria-valuenow="174" aria-valuemin="0" aria-valuemax="200"style=" width: 86.75%;">
							<span class="sr-only"></span></div>
						</div>          
				        <h5 align="left">대한민국 여자 평균키는161.1cm 입니다</h5> 
				      	<div class="progress">
							<div class="progress-bar progress-bar-warning" role="progressbar" 
								aria-valuenow="161" aria-valuemin="0" aria-valuemax="100" style="width: 80.55%;">
							<span class="sr-only"></span></div>
						</div>
					</td>
					<td>
						당신의 성씨는 <b><em>${su.ln_value}</em></b> 씨 입니다.<br><br>
						<h5>※ 다음은 2015년도 통계청 자료입니다</h5>
						<img alt="" src="${path }/images/lastName_percent1.jpg" width="300px">
					</td>
				</tr>
				<tr><td>
						당신이 키우고 싶어하는 반려동물은 <b><em>${su.pet_value}</em></b> 입니다.<br><br>
						<h5>※ 다음은 2014년도 반려동물 통계자료입니다</h5>
						<img alt="" src="${path }/images/pet.jpg" width="300px">
					</td>
					<td>
						당신이 여행가고 싶은 나라는 <b><em>${tour_value}</em></b> 입니다.<br><br>
						<h5>※ 다음은 2015년도 해외여행 희망나라 통계자료입니다</h5>
						<img alt="" src="${path }/images/tourSum.jpg" width="300px" height="200px">
					</td>
					<td>
						당신이 나이 들었다고 느끼는 순간은 바로 <br>
						<b><em>${old_value}</em></b> 입니다.<br><br>
						<h5>※ 다음은 연령대별 통계자료입니다</h5>
						<c:set var="age" value="${age_value/10 }" property=""></c:set>
						<fmt:parseNumber var="ageVal" integerOnly="True" value="${age }"></fmt:parseNumber>
						<c:if test="${ageVal == 2 }">
							<img alt="" src="${path }/images/old_20.png" width="300px">
						</c:if>
						<c:if test="${ageVal == 3 }">
							<img alt="" src="${path }/images/old_30.png" width="300px">
						</c:if>
						<c:if test="${ageVal >= 4 }">
							<img alt="" src="${path }/images/old_40.png" width="300px">
						</c:if>
					</td>
				</tr>
				<tr><td>
						당신이 여가시간에 주로 하는 활동은 <br>
						<b><em>${digital_value}</em></b> 입니다.<br><br>
						<h5>※ 다음은 일/공부 외 여유시간에 주로 하는 활동에 대한 2015년도 통계자료입니다</h5>
						<img alt="" src="${path }/images/digital_data.png" width="300px">
					</td>
					<td>
						당신이 주로 사용하는  SNS 콘텐츠는 <b><em>${mobile_value}</em></b> 입니다.<br><br>
						<h5>※ 다음은 2018년도 국내 SNS 이용자 통계현황입니다</h5>
						<img alt="" src="${path }/images/contents.jpg" width="300px">
					</td>
					<td>
						당신이 부자가 될 가능성에 대해 <b><em>${trans_value}</em></b> 라고 생각하는군요.<br><br>
						<h5>※ 다음은 2017년도 19세 이상 성인을 대상으로 조사한 결과입니다</h5>
						<img alt="" src="${path }/images/transfer.jpg" width="300px">
					</td>
				</tr>
				<tr><td>
						당신이 평소에 느끼는 행복의 정도는 <b><em>${happy_value}</em></b> 라고 생각하는군요.<br><br>
						<h5>※ 다음은 2017년도 20세 이상 성인을 대상으로 조사한 결과입니다</h5>
						<img alt="" src="${path }/images/happy_data.jpg" width="250px">
					</td>
					<td colspan="2" align="center">
						<br>
						그렇다면 행복의 조건은 무엇일까요?<br><br>
						<img alt="" src="${path }/images/happy2.png" width="100px">
						<br><br><br><br>
						<input type="submit" class="btn btn-danger" value="　  　다  음　▶　  " style="font-size: 16; font-color: #000000; font-weight: bold;"/>
						<div>&nbsp;</div>
					</td>
				</tr>
			</table>
		</div>
	</div>
</form>
</div>
<div class="container">
	<div align="center">
		서울 강남구 테헤란로 7길  DataCup Tel. 02-1234-5678 <br> 
		Copyright ⓒ 2019 by DataCup, Designed by DataCup
	</div>
</div>	
</body>
</html>
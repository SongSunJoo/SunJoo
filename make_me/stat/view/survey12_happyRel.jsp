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
<form action="survey1_main.do">
	<div class="header">
		<h3 style="font-style: italic; font-size:22px; font-family: cursive; font-weight: bold; color: #FFBB00;">DataCup2019</h3>
	</div>
	
	<div class="uppermain" align="center">
		<br>
		<div>
			<h3 style="color: navy; font-weight: bold;">행복지수 분석결과</h3>
			<!-- <section class="box1 pull-center" style="font-size:22px; font-family: serif; font-weight: bold; color: white;">
						행복지수 분석결과</section> -->
			<hr><br>
		</div>
		<div>
			<table class="table table-bordered" style="background: #EAEAEA">
				<tr><td align="center" > <span style="font-weight: bold; color: #BC2424;">성별</span> <span style="font-weight: bold; color: black;"> 행복지수 분석</span><br>
						<br>
						<img alt="" src="${path }/fileSave/happyRel1.png">
					</td>
				</tr>
				<tr>	
					<td align="center"><span style="font-weight: bold; color: #BC2424;">연령대별</span> <span style="font-weight: bold; color: black;"> 행복지수 분석</span><br>
						<br>
						<img alt="" src="${path }/fileSave/happyRel2.png">
					</td>
				</tr>
				<tr><td align="center"><span style="font-weight: bold; color: #BC2424;">키별</span> <span style="font-weight: bold; color: black;"> 행복지수 분석</span><br>
						<br>
						<img alt="" src="${path }/fileSave/happyRel3.png">
						<img alt="" src="${path }/fileSave/happyRel3-2.png">
					</td>
				</tr>
				<tr>
					<td align="center"><span style="font-weight: bold; color: #BC2424;">성씨별</span> <span style="font-weight: bold; color: black;"> 행복지수 분석</span><br>
						<br>
						<img alt="" src="${path }/fileSave/happyRel4.png">
					</td>
				</tr>
				<tr><td align="center"><span style="font-weight: bold; color: #BC2424;">키우고 싶은 반려동물별</span> <span style="font-weight: bold; color: black;"> 행복지수 분석</span><br>
						<br>
						<img alt="" src="${path }/fileSave/happyRel5.png">
					</td>
				</tr>
				<tr>
					<td align="center"><span style="font-weight: bold; color: #BC2424;">가고싶은 여행지별</span> <span style="font-weight: bold; color: black;"> 행복지수 분석</span><br>
						<br>
						<img alt="" src="${path }/fileSave/happyRel6.png">
					</td>
				</tr>
				<tr><td align="center"><span style="font-weight: bold; color: #BC2424;">여가시간 활동별</span> <span style="font-weight: bold; color: black;"> 행복지수 분석</span><br>
						<br>
						<img alt="" src="${path }/fileSave/happyRel7.png">
					</td>
				</tr>
				<tr>
					<td align="center"><span style="font-weight: bold; color: #BC2424;">주로 사용하는 SNS별</span> <span style="font-weight: bold; color: black;"> 행복지수 분석</span><br>
						<br>
						<img alt="" src="${path }/fileSave/happyRel8.png">
					</td>
				</tr>
				<tr><td colspan="2" align="center">
						<br><br>
						<input type="submit" class="btn btn-warning" value="　  　홈으로　▶　  " style="font-size: 16; font-color: #000000; font-weight: bold;"/>
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
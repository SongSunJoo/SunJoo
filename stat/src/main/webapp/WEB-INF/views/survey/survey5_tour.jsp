<%@ page language="java" contentType="text/html; charset=UTF-8"
    pageEncoding="UTF-8"%>
<%@ include file="../header.jsp" %>
<!DOCTYPE html>
<html>
<head>
<meta charset="UTF-8">
<title>Insert title here</title>
</head>
<body>
<div class="container">	
<form action="survey5R_tour.do">
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
			<div class="col-md-2"></div>
			<div class="col-md-8" align="center">
				<br>
				<h4 style="color: white; font-weight: bold;">당신이 가고 싶은 여행지는?</h4>
				<hr>
				<table class="tour_tab">
					<tr>
						<td align="center"><img class="tour_img" alt="" src="${path}/images/france.png" ></td>
						<td align="center"><img class="tour_img" alt="" src="${path}/images/hawaii.png"></td>
						<td align="center"><img class="tour_img" alt="" src="${path}/images/australia.png"></td>
						<td align="center"><img class="tour_img" alt="" src="${path}/images/usa.png"></td>
						<td align="center"><img class="tour_img" alt="" src="${path}/images/switzerland.png"></td>
						<td align="center"><img class="tour_img" alt="" src="${path}/images/italy.png"></td>
						<td align="center"><img class="tour_img" alt="" src="${path}/images/japan.png"></td>
						<td align="center"><img class="tour_img" alt="" src="${path}/images/uk.png"></td>
						<td align="center"><img class="tour_img" alt="" src="${path}/images/hongkong.png"></td>
						<td align="center"><img class="tour_img" alt="" src="${path}/images/newzealand.png"></td>
					</tr>
					<tr>
						<td align="center"><label id="tour"><input type="radio" name="tourDB_id1" value=1 checked="checked">프랑스</label></td>
						<td align="center"><label id="tour"><input type="radio" name="tourDB_id1" value=2 >하와이</label></td>
						<td align="center"><label id="tour"><input type="radio" name="tourDB_id1" value=3 >호주</label></td>
						<td align="center"><label id="tour"><input type="radio" name="tourDB_id1" value=4 >미국</label></td>
						<td align="center"><label id="tour"><input type="radio" name="tourDB_id1" value=5 >스위스</label></td>
						<td align="center"><label id="tour"><input type="radio" name="tourDB_id1" value=6 >이탈리아</label></td>
						<td align="center"><label id="tour"><input type="radio" name="tourDB_id1" value=7 >일본</label></td>
						<td align="center"><label id="tour"><input type="radio" name="tourDB_id1" value=8 >영국</label></td>
						<td align="center"><label id="tour"><input type="radio" name="tourDB_id1" value=9 >홍콩</label></td>
						<td align="center"><label id="tour"><input type="radio" name="tourDB_id1" value=10 >뉴질랜드</label></td>
					</tr>
				</table>
				<div>&nbsp;</div>
				<br>
				<input type="submit" class="btn btn-danger" value="　  　다  음　▶　  " style="font-size: 16; font-color: #000000; font-weight: bold;"/>
				<div>&nbsp;</div>
			</div>
			<div class="col-md-2"></div>
		</div>
	</div>
</form>
</div>
</body>
</html>
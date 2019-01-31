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
<form action="survey8R_content.do">
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
			<div class="col-md-1"></div>
			<div class="col-md-10" align="center">
				<br>
				<h4 style="color: white; font-weight: bold;">자주 사용하는 SNS 콘텐츠는?</h4>
				<hr>
				<table class="ani_tab">
					<tr>
						<td align="center"><img class="ani_img" alt="" src="${path}/images/band.png" ></td>
						<td align="center"><img class="ani_img" alt="" src="${path}/images/instagram.png"></td>
						<td align="center"><img class="ani_img" alt="" src="${path}/images/facebook.png"></td>
						<td align="center"><img class="ani_img" alt="" src="${path}/images/kakaostory.jpg"></td>
						<td align="center"><img class="ani_img" alt="" src="${path}/images/navercafe.jpg"></td>
						<td align="center"><img class="ani_img" alt="" src="${path}/images/daumcafe.jpg"></td>
						<td align="center"><img class="ani_img" alt="" src="${path}/images/naverblog.png"></td>
						<td align="center"><img class="ani_img" alt="" src="${path}/images/twitter.png"></td>
						<td align="center"><img class="ani_img" alt="" src="${path}/images/everytime.jpg"></td>
						<td align="center"><img class="ani_img" alt="" src="${path}/images/cashslide.png"></td>
						
						
					</tr>
					<tr>
						<td align="center" style="font-size: 13px;"><label id="mobile"><input type="radio" name="mobileDB_id" value=1 id="mobile" checked="checked">밴드</label></td>
						<td align="center" style="font-size: 13px;"><label id="mobile"><input type="radio" name="mobileDB_id" value=2 id="mobile">인스타그램</label></td>
						<td align="center" style="font-size: 13px;"><label id="mobile"><input type="radio" name="mobileDB_id" value=3 id="mobile">페이스북</label></td>
						<td align="center" style="font-size: 13px;"><label id="mobile"><input type="radio" name="mobileDB_id" value=4 id="mobile">카카오스토리</label></td>
						<td align="center" style="font-size: 13px;"><label id="mobile"><input type="radio" name="mobileDB_id" value=5 id="mobile">네이버카페</label></td>
						<td align="center" style="font-size: 13px;"><label id="mobile"><input type="radio" name="mobileDB_id" value=6 id="mobile">다음카페</label></td>
						<td align="center" style="font-size: 13px;"><label id="mobile"><input type="radio" name="mobileDB_id" value=7 id="mobile">네이버블로그</label></td>
						<td align="center" style="font-size: 13px;"><label id="mobile"><input type="radio" name="mobileDB_id" value=8 id="mobile">트위터</label></td>
						<td align="center" style="font-size: 13px;"><label id="mobile"><input type="radio" name="mobileDB_id" value=9 id="mobile">에브리타임</label></td>
						<td align="center" style="font-size: 13px;"><label id="mobile"><input type="radio" name="mobileDB_id" value=10 id="mobile">캐시슬라이드</label></td>
						
					</tr>
				</table>
				<!-- <div>&nbsp;</div> -->
				<br>
				<input type="submit" class="btn btn-danger" value="　  　다  음　▶　  " style="font-size: 16; font-color: #000000; font-weight: bold;"/>
				<div>&nbsp;</div>
			</div>
			<div class="col-md-1"></div>
		</div>
	</div>
</form>
</div>
</body>
</html>
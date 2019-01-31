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
<form action="survey4R_animal.do">
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
				<h4 style="color: white; font-weight: bold;">당신이 키우고 싶은 반려동물은?</h4>
				<hr>
				<table class="ani_tab">
					<tr>
						<td align="center"><img class="ani_img" alt="" src="${path}/images/dog.png" ></td>
						<td align="center"><img class="ani_img" alt="" src="${path}/images/cat.png"></td>
						<td align="center"><img class="ani_img" alt="" src="${path}/images/bird.png"></td>
						<td align="center"><img class="ani_img" alt="" src="${path}/images/rabbit.png"></td>
						<td align="center"><img class="ani_img" alt="" src="${path}/images/hamster.png"></td>
						<td align="center"><img class="ani_img" alt="" src="${path}/images/fish.png"></td>
						<td align="center"><img class="ani_img" alt="" src="${path}/images/turtle.png"></td>
						<td align="center"><img class="ani_img" alt="" src="${path}/images/other_pet.png"></td>
						<td align="center"><img class="ani_img" alt="" src="${path}/images/no-pets.png"></td>
					</tr>
					<tr>
						<td align="center"><label id="pet"><input type="radio" name="pet_value" value="개" checked="checked">강아지</label></td>
						<td align="center"><label id="pet"><input type="radio" name="pet_value" value="고양이" id="pet">고양이</label></td>
						<td align="center"><label id="pet"><input type="radio" name="pet_value" value="새" id="pet">새</label></td>
						<td align="center"><label id="pet"><input type="radio" name="pet_value" value="토끼" id="pet">토끼</label></td>
						<td align="center"><label id="pet"><input type="radio" name="pet_value" value="햄스터" id="pet">햄스터</label></td>
						<td align="center"><label id="pet"><input type="radio" name="pet_value" value="물고기" id="pet">물고기</label></td>
						<td align="center"><label id="pet"><input type="radio" name="pet_value" value="거북이" id="pet">거북이</label></td>
						<td align="center"><label id="pet"><input type="radio" name="pet_value" value="기타" id="pet">기타</label></td>
						<td align="center"><label id="pet"><input type="radio" name="pet_value" value="없음" id="pet">없음</label></td>
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
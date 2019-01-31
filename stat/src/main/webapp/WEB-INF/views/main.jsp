<%@ page language="java" contentType="text/html; charset=UTF-8"
    pageEncoding="UTF-8"%>
<!DOCTYPE html>
<html>
<head>
<meta charset="UTF-8">
<title>DataCup2019</title>
<style type="text/css">
	body {
		background-color: #C2E2E8;
	}
	.uppermain {
		width: 80%;
		margin: 50px 50px;
	}
	.lowerleft {
		margin: 30px 30px;
		float: left;
	}
	.lowerright {
		margin: 30px 30px;
	}

</style>
</head>
<body>
<div class="container">
	<div class="header">
		<h3>DataCup2019</h3>
	</div>
		<div class="uppermain" align="center">
			<form>
			<table border="1">
				<tr><td colspan="2" align="center">Let's talk about you!</td></tr>
				<tr><td colspan="2" align="center">
					아래 항목에 해당사항을 입력해주세요<br>
					10가지의 질문을 끝내면 당신에 관한 리포트를 출력해드립니다</td></tr>
				<tr><td width="150" align="center">지역</td>
					<td><select name="area">
						<option value=seoul>서울시</option>
						<option value=gyunggi>경기도</option>
						<option value=>강원도</option>
						<option value=>충청북도</option>
						<option value=>충청남도</option>
						<option value=>전북</option>
						<option value=>전라남도</option>
						<option value=>경상북도</option>
						<option value=>경상남도</option>
						<option value=>제주도</option>
						<option value=>해외</option></select></td></tr>
				<tr><td width="150" align="center">성별</td>
					<td><input type ="radio" name="men" value="red"/>남성
						<input type ="radio" name="women" value="red"/>여성</td></tr>
				<tr><td width="150" align="center">나이</td>
					<td><input type="text" name="age"></input></td></tr>
				<tr><td width="150" align="center">직업</td>
					<td><select name="job">
						<option value=>초중고학생</option>
						<option value=>대학생</option>
						<option value=>직장인</option>
						<option value=>자영업</option>
						<option value=>무직</option>
						</select></td></tr>
				<tr><td colspan="2" align="center">
					<input type="submit" class="btn btn-warning" value="Submit"/></td></tr>
			</table>
		</div>
		<div class="lowermain">
			<span class="lowerleft" align="center">
				<table border=1>
					<tr><td colspan="3">* Poll</td></tr>
					<tr><td width="100">A</td><td>VS</td><td width="100">BBBBBBBBBBBBB</td>
				</table>
			<span class="lowerright" align="center"><table border=1>
					<tr><td colspan="3">* FUN</td></tr>
					<tr><td width="150">AAAAAAAAAA</td><td width="150">BBBBBBBBBBBBB</td>
				</table>
			</span>	
		</div>
	<div class="footer">footer</div>
</div>
</body>
</html>
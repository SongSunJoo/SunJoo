<%@ page language="java" contentType="text/html; charset=UTF-8"
    pageEncoding="UTF-8"%>
<%@ include file="../header.jsp" %>
<!DOCTYPE html>
<html>
<head>
<meta charset="UTF-8">
<title>DataCup2019</title>
<style type="text/css">
	body {
		background-color: #4FADC2;
	}
 	.uppermain {
		/* width: 80%; */
		margin: 50px 50px;
		background-color: #7DC9DB;
	} 
  	.lowerleft {
		margin: 30px 30px;
		float: left;
	} 
 	.lowerright {
		margin: 30px 30px;
	}
	.row {
		padding: 10px 10px;
	}

</style>
</head>
<body>
<div class="container">
	<div class="header">
		<h3 style="font-style: italic; font-size:22px; font-family: cursive; font-weight: bold; color: #FFBB00;">DataCup2019</h3>
		<!-- <h3 class="text-primary">DataCup 2019</h3> -->
	</div>
	
	<div class="uppermain" align="center">
		<div class="row">
			<div class="col-md-3"></div>
			<div class="col-md-6">
				<form action="survey2R_height.do">
					<input type="hidden" name="user_id" value="${user_id }">
					<br>
					<div>
						<section class="box1 pull-center" style="font-size:22px; font-family: serif; font-weight: bold; color: white;">내 키는 몇 등?</section>
						<hr><br>
					</div>
					
					<div>
						<table style="width: 100%">
							<tr>
								<td><img alt="" src="${path }/images/height.png" width="35px"></td>
								<td width="80%">
									<input type="number" name="height_value" class="form-control" placeholder="키를 입력하세요" required="required" autofocus="autofocus"></td>
								<td><font style="font-size: 20; font-weight: bold;">　cm</font></td>
							</tr>
						</table>
					</div>
					<div class="row">
						<br><br>당신의 키를 입력해주세요<br>
						반에서 몇번째로 큰 사람인지 알려드립니다<br><br><br>
						<input type="submit" class="btn btn-danger" value="　  　다  음　▶　  " style="font-size: 16; font-color: #000000; font-weight: bold;"/>
						<br>
					</div>
				</form>
			</div>
			<div class="col-md-3"></div>
		</div>
	</div>
</div>
<div class="container">
	<div align="center">
		서울 강남구 테헤란로 7길  DataCup Tel. 02-1234-5678 <br> 
		Copyright ⓒ 2019 by DataCup, Designed by DataCup
	</div>
</div>	
	
</body>
</html>
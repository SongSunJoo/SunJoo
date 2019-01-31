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
	<div class="header">
		<h3 style="font-style: italic; font-size:22px; font-family: cursive; font-weight: bold; color: #FFBB00;">DataCup2019</h3>
		<!-- <h3 class="text-primary">DataCup 2019</h3> -->
	</div>
	<div align="center" style="margin: 30px 30px;">
		<div class="row">
		<div class="col-md-2"></div>
		<div class="col-md-8" style="background-color: #153E5C">			
			<form action="survey1R_life.do">
				<br>
				<div>
					<section class="box1 pull-center" style="font-style: italic; font-size:22px; font-family: serif; font-weight: bold; color: white;">Let's talk about you!</section>
				</div>
				<div>
					<div class="row">
					<div class="col-md-3"></div>
					<div class="col-md-6"><hr></div>
					<div class="col-md-3"></div>
					</div>
				</div>
				<div>
					<div class="row">
					<div class="col-md-2"></div>
					<div class="col-md-8" style="color: white;">아래 항목에 해당사항을 입력해주세요</div>
					<div class="col-md-2"></div>
					</div>
				</div>
				<div>
					<div class="row">
					<div class="col-md-2"></div>
					<div class="col-md-8" style="color: white;" align="center">10가지의 질문을 끝내면 당신에 관한 리포트를 출력해 드립니다</div>
					<div class="col-md-2"></div>
					</div>
				</div><br>
				<div>
					<div class="row">
					<div class="col-md-2"></div>
					<div class="col-md-8">	
						<table class="table" style="color: white;">
							<tr><td width="150" align="center">지역</td>
								<td>
									<div class="col-xs-10 col-lg-10">
									<select name="place" class="form-control">
										<option value="서울시">서울시</option>
										<option value="경기도">경기도</option>
										<option value="강원도">강원도</option>
										<option value="충청북도">충청북도</option>
										<option value="충청남도">충청남도</option>
										<option value="전라북도">전라북도</option>
										<option value="전라남도">전라남도</option>
										<option value="경상북도">경상북도</option>
										<option value="경상남도">경상남도</option>
										<option value="제주도">제주도</option>
										<option value="해외">해외</option></select></div>
								</td></tr>
							<tr><td width="150" align="center">성별</td>
								<td>
									<div class="col-xs-10 col-lg-10">
									<label class="radio-inline">
										<input type ="radio" name="gender" id="gender" value="남"/>남성
									</label>
									<label class="radio-inline">
										<input type ="radio" name="gender" id="gender" value="여"/>여성
									</label></div>
								</td></tr>	
							<tr><td width="150" align="center">나이</td>
								<td>
								<div class="form-group">   
							        <div class="col-xs-10 col-lg-10">
							            <input type="number" class="form-control" id="age" name="age" required="required"> 
							        </div>
							    </div>
								<!-- <input type="number" name="age" required="required"></input> -->
								</td></tr>
							<tr><td width="150" align="center">직업</td>
								<td>
									<div class="col-xs-10 col-lg-10">
									<select name="group" class="form-control">
									<option value="초중고학생">초중고학생</option>
									<option value="대학생">대학생</option>
									<option value="직장인">직장인</option>
									<option value="자영업">자영업</option>
									<option value="무직">무직</option>
									</select></div>
								</td></tr>
							<tr><td colspan="2" align="center">
								<input type="submit" class="btn btn-danger" value="　  　다  음　▶　  " style="font-size: 16; font-color: #000000; font-weight: bold;"/>
						</table>
						<div align="center" style="font-size: 10px; color: white;">
							※ 수집된 정보는 오직 통계자료 목적으로만 활용됩니다
						</div>
					</div>
					<div class="col-md-2"></div>
					</div>
				</div>
			</form>
		</div>
		<div class="col-md-2"></div>
		</div>
	</div>
</div>
<div class="container">
	<div class="lowermain">
		<div class="col-md-6" align="center" style="border-style: solid; border-width: thin;">
			
			<div class="col-xs-10 col-lg-10">
				poll 영역
			</div>
		</div>
		<div class="col-md-6" align="center" style="border-style: solid; border-width: thin;">
			재미있는 통계 영역
		</div>
	</div><br>
	<div align="center">
		서울 강남구 테헤란로 7길  DataCup Tel. 02-1234-5678 <br> 
		Copyright ⓒ 2019 by DataCup, Designed by DataCup
	</div>
</div>	
	
</body>
</html>
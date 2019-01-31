<%@ page language="java" contentType="text/html; charset=UTF-8"
    pageEncoding="UTF-8"%>
<%@ include file="../header.jsp" %>
<!DOCTYPE html>
<html>
<head>
<meta charset="UTF-8">
<title>DataCup2019</title>
<style type="text/css">
	.lower-row, table {
		/* width: 80%; */
		padding: 20px 20px;
	}
</style>
<script type="text/javascript">
	function mainChk() {
		if(frm.age.value < 0) {
			alert("나이는 음수로 등록 할 수 없습니다. 다시 입력해주세요.");
			frm.age.value = "";
			return false;
		}
	}
</script>
</head>
<body>
<div class="container">
	<div class="header" style="position:relative;height:40px">
		<h3 style="font-style: italic; font-size:22px; font-family: cursive; font-weight: bold; color: #FFBB00;">DataCup2019</h3>
		<!-- <h3 class="text-primary">DataCup 2019</h3> -->
	</div>
	<div align="center" style="position:relative;height:500px">
		<div class="row">
		<div class="col-md-2"></div>
		<div class="col-md-8" style="background-color: #153E5C">			
			<form action="survey1R_life.do" id="frm" name="frm" onsubmit="return mainChk()">
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
										<input type ="radio" name="gender" id="gender" value="man" checked="checked"/>남성
									</label>
									<label class="radio-inline">
										<input type ="radio" name="gender" id="gender" value="woman"/>여성
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
									<option value="student">초중고학생</option>
									<option value="university">대학생</option>
									<option value="worker">직장인</option>
									<option value="owner">자영업</option>
									<option value="none">무직</option>
									</select></div>
								</td></tr>
							<tr><td colspan="2" align="center">
								<input type="submit" class="btn btn-warning" value="다음" style="color: navy; font-weight: bold;"/></td></tr>
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
		</div>
	</div>
</div>

<div class="container" style="position:relative; height:350px;">
	<div class="col-md-7" align="center">
		<div class="lower-left">
			<div class="lower-left-title" align="left">
				<font size="3" style="font-weight:bold;"> [데이터 만들기] 우리가 만드는 데이터 월드컵  </font>
			</div>
			<div class="poll_board_plus" align="right">
					<a href="poll_insertForm.do"> + 설문조사 등록하기</a></div><br>
			<div class="lowerleft-main">
				<!-- 만약에 게시글이 없을 때 -->
				<c:if test="${empty ongoinglist}">
					<table border="0" style="background-color: #B3CFD4;">
						<tr><th colspan="3">게시글이 없습니다</th></tr>
					</table>
				</c:if>

				<!-- 만약에 list가 empty가 아닐때, 반복문 실행 -->
				<c:if test="${not empty ongoinglist}">
					<!-- list와 total(전체 갯수) 불러옴 -->
					<c:forEach var="poll" items="${ongoinglist}">
						<table style="background-color: #B3CFD4;">
							<form action="poll_result.do">
							<input type="hidden" name="poll_id" value="${poll.poll_id}">
							<input type="hidden" name="poll_cont1" value="${poll.poll_cont1}">							
							<input type="hidden" name="poll_cont2" value="${poll.poll_cont2}">
							<input type="hidden" name="poll_cont1_cnt" value="${poll.poll_cont1_cnt}">							
							<input type="hidden" name="poll_cont2_cnt" value="${poll.poll_cont2_cnt}">							
							<tr height="40">
								<td colspan="2" >
									<font size="4" style="font-weight:bold;">&nbsp;&nbsp;Q. ${poll.poll_title}</font>
								</td>
								<td colspan="1" align="center">
									<font size="2"> 기간 : ${poll.poll_startDate} ~ ${poll.poll_endDate}</font>
								</td>
							</tr>
							<tr height="65">
								<td width="300" align="center">
									<label>
										<input type="radio" id="poll_cont1" name="poll_cont" value="${poll.poll_cont1}" checked="checked"/>
										${poll.poll_cont1}
									</label>
								</td>
								<td width="50" align="center">
									<font size="6" color="#D9534F" style="font-weight:bold;">VS</font>
								</td>
								<td width="300" align="center">
									<label>
										<input type="radio" id="poll_cont2" name="poll_cont" value="${poll.poll_cont2}"/>
										${poll.poll_cont2}
									</label>
								</td>
							</tr>
							<tr height="20">
								<td colspan="5" align="center">
									<input type="submit" class="btn btn-warning" value="투표하고 결과보기" style="color: white; font-weight: bold;"/>
								</td>
							</tr>
							<tr height="5">
								<td colspan="5">&nbsp;</td>
							</tr>
							</form>
						</table><br>
					</c:forEach>
				</c:if>
			</div>
		</div>
	</div>
		<div class="col-md-5" align="center">
			<div class="lower-right">
				<div class="lower-right-title" align="left">
					<font size="3" style="font-weight:bold;"> [데이터 즐기기] 재미있는 통계 이야기 </font>
					<br>
					<br><br>
				</div>
				<div class="lower-right-main">
					<table class="table table-hover" border="0" style="background-color: #B3CFD4;">
						<tr>
							<td height="90" align="center" style="padding: 30px">
								<img src="${path }/images/virtual-reality.png" width="50px">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
									<a href="poll_fun1.do">통계로 보는 2054 한국</a>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
								<img src="${path }/images/virtual-reality.png" width="50px"></td></tr>
						<tr>
							<td height="90" align="center" style="padding: 30px">
								<img src="${path }/images/laugh.png" width="50px">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
									<a href="poll_fun2.do">통계로 보는 전세계 남녀 평균 키</a>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
								<img src="${path }/images/laugh.png" width="50px"></td></tr>
						<tr>
							<td height="90" align="center" style="padding: 30px">
								<img src="${path }/images/happiness.png" width="50px">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
									<a href="poll_fun3.do">통계로 보는 전세계 행복 지수</a>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
								<img src="${path }/images/happiness.png" width="50px">
									</td></tr>
					</table>
				</div>
			</div>
		</div>
	</div>

<div class="col-md-12" style="position:relative; bottom:0px;" align="center">
	서울 강남구 테헤란로 7길  DataCup Tel. 02-1234-5678 <br> 
	Copyright ⓒ 2019 by DataCup, Designed by DataCup
</div>
</div>
</body>
</html>
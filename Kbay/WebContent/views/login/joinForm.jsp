<%@ page language="java" contentType="text/html; charset=UTF-8"
	pageEncoding="UTF-8"%>
<!DOCTYPE html>
<html>
<head> 
<meta charset="UTF-8">
<meta http-equiv="X-UA-Compatible" content="IE=edge">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Insert title here</title>
<link href="../../css/bootstrap.min.css" rel="stylesheet">
<script type="text/javascript" src="../../js/jquery.js"></script>
<script src="../../js/bootstrap.min.js"></script>
<script type="text/javascript">
	function chk() {
		var a = $('#idCheck').val();
		if (a == 'n') {
			alert("아이디를 체크하세요");
			$('#id').focus();
			$("#btn_ok").attr("disabled", "disabled");
			return false;
		}
	}
	//1. id 중복 체크 및 id 체크
	$(function() {	
		$('#idChk').click(function() {
			var id = $('#id2').val();
		//	alert(id);
			if (id == null || id == "" || id == " ") {
				alert("아이디를 입력하세요");
				$('#id').focus();
				$("#btn_ok").attr("disabled", "disabled");
				return false;
			}
			if (id != null) {
				$.post('idConfirmResult.do', 'id=' + id,
					function(data) {
						//console.log(data);
						$('#disp').html(data);
						$('#disp').css('color', 'red');
						$('#idCheck').val('y');
					});
			} 
		});

		// 2. 비밀번호 체크
		$("#alert-success").hide();
		$("#alert-danger").hide();
		$("input").keyup(function() {
			var pass = $("#pass").val();
			var pass2 = $("#pass2").val();
			if (pass != "" || pass2 != "") {
				if (pass == pass2) {
					$("#alert-success").show();
					$("#alert-danger").hide();
					$("#btn_ok").removeAttr("disabled");
				} else {
					$("#alert-success").hide();
					$("#alert-danger").show();
					$("#btn_ok").attr("disabled", "disabled");
				}
			}
		});
	});
	/* 	// 1. id 중복 체크
	 function idConfirm() {
	 // 만약 아래 Form 'frm'의 id 값이 없으면, alert창
	 if (!frm.id.value) {
	 alert("아이디를 입력해주세요");
	 // frm의 id에 커서 옮겨줌
	 frm.id.focus();
	 // 다음으로 넘어가지 못하게 false
	 return false;
	 }
	 // 만약 id 중복 확인 성공하면, idConfirm으로 연결해 새창 띄워줌
	 // ?id="+frm.id.value는 아래 Form 'frm의 id값을 가지고 'idConfirm.do'로 넘거간다는 뜻
	 window.open("idConfirmResult.do?id="+frm.id.value, "idConfirm", "width=300 height=300");
	 }
	 // 2. 비밀번호 중복 체크
	 function passConfirm() {
	 if(frm.pass.value != frm.pass2.value) {
	 // pass
	 alert("비밀번호가 일치하지 않습니다.");
	 frm.pass2.value = "";
	 frm.pass2.focus();
	 return false;
	 }
	 } */
</script>
<base target="_self" />
</head>
<body>
	<div class="container">
		<div id="content" align="center">
			<div class="row">

				<!-- md : pc 크기의 컬럼을 2개 차지 -->
				<div class="col-md-2"></div>
				<div class="col-md-8">
					<h3>회원가입</h3>

					<form action="join.do" name="frm" id="frm" role="form"
						class="form-horizontal" method="post" onsubmit="return chk()">
						<input type="hidden" name="idCheck" id="idCheck" value="n">
						<div class="form-group">
							<label for="id" class="col-xs-2 col-lg-2 control-label">아이디</label>
							<div class="col-xs-10 col-lg-10">
								<div class="row">
									<div class="col-md-8">
										<input type="text" class="form-control" placeholder="아이디"
											id="id2" name="id" required="required" autofocus="autofocus">
										<div id="disp" class="err"></div>
									</div>
									<div class="col-md-48">
										<!-- <button type="button"  class="btn btn-default"  name="idChk" id="idChk">중복확인</button> -->
										<input type="button" class="btn btn-default" data-toggle="modal" id="idChk"
											name="idChk" value="중복확인">
									</div>
									
								</div>
							</div>
						</div>
						<div class="form-group">
							<label for="pass" class="col-xs-2 col-lg-2 control-label">암호</label>
							<div class="col-xs-10 col-lg-10">
								<input type="password" class="form-control" placeholder="암호"
									name="pass" id="pass" required="required">
							</div>
						</div>
						<div class="form-group">
							<label for="pass2" class="col-xs-2 col-lg-2 control-label">암호확인</label>
							<div class="col-xs-10 col-lg-10">
								<input type="password" class="form-control" placeholder="암호확인"
									name="pass2" id="pass2" required="required">
							</div>
							<div class="alert alert-success" id="alert-success">비밀번호가
								일치합니다.</div>
							<div class="alert alert-danger" id="alert-danger">비밀번호가
								일치하지 않습니다.</div>
						</div>
						<div class="form-group">
							<label for="name" class="col-xs-2 col-lg-2 control-label">이름</label>
							<div class="col-xs-10 col-lg-10">
								<input type="text" class="form-control" placeholder="이름"
									name="name" required="required">
							</div>
						</div>
						<div class="form-group">
							<label for="email" class="col-xs-2 col-lg-2 control-label">이메일</label>
							<div class="col-xs-10 col-lg-10">
								<input type="email" class="form-control" placeholder="이메일"
									name="email" required="required">
							</div>
						</div>
						<div class="form-group">
							<label for="tel" class="col-xs-2 col-lg-2 control-label">전화번호</label>
							<div class="col-xs-10 col-lg-10">
								<input type="tel" class="form-control"
									pattern="\d{3}-\d{3,4}-\d{4}" placeholder="010-1234-1234"
									name="tel" required="required">
							</div>
						</div>
						<div class="form-group">
							<label for="addr" class="col-xs-2 col-lg-2 control-label">주소</label>
							<div class="col-xs-10 col-lg-10">
								<input type="text" class="form-control" placeholder="주소"
									name="addr" required="required">
							</div>
						</div>
						<div class="form-group">
							<div class="row">
								<div class="col-md-2"></div>
								<div class="col-md-8">
									<input type="submit" class="btn btn-default" id="btn_ok" value="가입하기">
									<button type="button" class="btn btn-default"
										onclick="history.go(-1)">취소</button>
									<button type="reset" class="btn btn-default">다시작성</button>
								</div>
								<div class="col-md-2"></div>
							</div>
						</div>



						<!-- <table>
	<tr><th width="120pt">아이디</th>
		<td width="200pt"><input type="text" name="id" required="required" autofocus="autofocus"></td>
		<td width="120pt"><input type="button" name="idChk" value="아이디 중복 확인" onclick="return idConfirm()"></td></tr>
	<tr><th>비밀번호</th>
		<td><input type="password" name="pass" required="required"></td>
	<tr><th>비밀번호 확인</th>
		<td><input type="password" name="pass2" required="required"></td>
	<tr><th>이름</th>
		<td><input type="text" name="name" required="required"></td></tr>
	<tr><th>이메일</th>
		<td><input type="email" name="email" required="required"></td></tr>
	<tr><th>전화번호</th>
		<td><input type="tel" name="tel" required="required"
		pattern="\d{3}-\d{3,4}-\d{4}" placeholder="010-1234-1234"></td></tr>
	<tr><th>주소</th>
		<td><input type="text" name="addr" required="required"></td></tr>
	<tr><th colspan="3">　</th></tr>
	<tr><th colspan="3" align="center">
		<input type="button" value="취소하기" name="cancel" onclick="history.go(-1)">
		<input type="reset" value="다시작성" name="reset">
		<input type="submit" value="가입하기"></th></tr>
	</table> -->
					</form>
				</div>
				<div class="col-md-2"></div>
			</div>
		</div>
	</div>
</body>
</html>
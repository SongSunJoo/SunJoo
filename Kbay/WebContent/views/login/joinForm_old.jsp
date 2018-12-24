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
	// 1. id 중복 체크
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
	}
</script>
</head>
<body>
<div class="container"> 
	<div id="content" align="center">
	<div class="row">

	<!-- md : pc 크기의 컬럼을 2개 차지 -->
	<div class="col-md-2"></div>
	<div class="col-md-8">
	<h3>회원가입</h3>
		
	<form action="join.do" name="frm" role="form" class="form-horizontal" method="post" onsubmit="return passConfirm()">
		<div class="form-group">   
	        <label for="id"  class="col-xs-2 col-lg-2 control-label">아이디</label>
	        <div class="col-xs-10 col-lg-10">
	        	<div class="row">
					<div class="col-md-8">
		            	<input type="text" class="form-control" placeholder="아이디" name="id" required="required" autofocus="autofocus"> 
		        	</div>
		        	<div class="col-md-48">
		        		<button type="button"  class="btn btn-default"  name="idChk" onclick="return idConfirm()"> 중복확인</button>
		        	</div>
	        	</div>
	        </div>
	    </div>       
	   	<div class="form-group">  
	        <label for="pass"  class="col-xs-2 col-lg-2 control-label">암호</label>
	   	    <div class="col-xs-10 col-lg-10">
	            <input type="password" class="form-control" placeholder="암호" name="pass" required="required" > 
	        </div> 
	   	</div> 
	   	<div class="form-group">  
	        <label for="pass2"  class="col-xs-2 col-lg-2 control-label">암호확인</label>
	   	    <div class="col-xs-10 col-lg-10">
	            <input type="password" class="form-control" placeholder="암호확인" name="pass2" required="required" > 
	        </div> 
	   	</div> 
	   	<div class="form-group">  
	        <label for="name"  class="col-xs-2 col-lg-2 control-label">이름</label>
	   	    <div class="col-xs-10 col-lg-10">
	            <input type="text" class="form-control" placeholder="이름" name="name" required="required" > 
	        </div> 
	   	</div> 
	   	<div class="form-group">  
	        <label for="email"  class="col-xs-2 col-lg-2 control-label">이메일</label>
	   	    <div class="col-xs-10 col-lg-10">
	            <input type="email" class="form-control" placeholder="이메일" name="email" required="required" > 
	        </div> 
	   	</div> 
	   	<div class="form-group">  
	        <label for="tel"  class="col-xs-2 col-lg-2 control-label">전화번호</label>
	   	    <div class="col-xs-10 col-lg-10">
	            <input type="tel" class="form-control" pattern="\d{3}-\d{3,4}-\d{4}" placeholder="010-1234-1234" name="tel" required="required" > 
	        </div> 
	   	</div>
	   	<div class="form-group">  
	        <label for="addr"  class="col-xs-2 col-lg-2 control-label">주소</label>
	   	    <div class="col-xs-10 col-lg-10">
	            <input type="text" class="form-control" placeholder="주소" name="addr" required="required" > 
	        </div> 
	   	</div>  
		<div class="form-group"> 
  		 	<div class="row"> 
	  		 	<div class="col-md-2"></div>
	  		 	<div class="col-md-8">   
	        	<button type="submit"  class="btn btn-default"> 가입하기</button>   
	        	<button type="button"  class="btn btn-default" onclick="history.go(-1)"> 취소</button>
	        	<button type="reset"  class="btn btn-default"> 다시작성</button>
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
</body>
</html>
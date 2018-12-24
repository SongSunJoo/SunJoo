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

<script>
			// 변수명 
	function findId() {
			// 새창으로 띄우려는 곳의 경로값, 새창 이름, 넓이 & 높이
		window.open("findId.jsp", "findId", "width=300 height=300");
	}
	function findPass() {
		window.open("findPass.jsp", "findPass", "width=300 height=300");
	}
</script>
</head>
<body>
	<div class="container"> 
		<div id="content" align="center">
		<div class="row">
	
		<!-- md : pc 크기의 컬럼을 2개 차지 -->
		<div class="col-md-3"></div>
		<div class="col-md-5">
		<h3>로그인</h3>
		<form role="form" class="form-horizontal" action="../login/login.do" name="frm" method="post">  
		    <div class="form-group">   
		        <label for="Name"  class="col-xs-2 col-lg-2 control-label">아이디</label>
		        <div class="col-xs-10 col-lg-10">
		            <input type="text" class="form-control" placeholder="아이디" name="id" required="required" autofocus="autofocus"> 
		        </div>
		    </div>       
	    	<div class="form-group">  
		        <label for="Passward"  class="col-xs-2 col-lg-2 control-label">암호</label>
	    	    <div class="col-xs-10 col-lg-10">
		            <input type="password" class="form-control" placeholder="암호" name="pass" required="required" > 
		        </div> 
	    	</div> 
	  		 <div class="form-group"> 
	  		 	<div class="row"> 
		  		 	<div class="col-md-2"></div>
		  		 	<div class="col-md-4">   
		        	<button type="submit"  class="btn btn-default"> 로그인</button>   
		        	<button type="button"  class="btn btn-default" onclick="history.go(-1)"> 취소</button>
		        	</div>
		        	<div class="col-md-1"></div>
		        	<div class="col-md-5">   
		        	<button type="button"  class="btn btn-default" onclick="findId()"> 아이디찾기</button>
		        	<button type="button"  class="btn btn-default" onclick="findPass()"> 암호찾기</button>
		        	</div>
		        	<!-- <div class="col-md-1"></div> 
	        	</div> -->
	    	</div>              
	    </form> 
	    </div>
	    <div class="col-md-4"></div>
	</div>
	</div>
	</div>

</body>
</html>
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
	
</script>
</head>
<body>
<div class="container"> 
	<div id="content" align="center">
	<div class="row">
		<div class="col-md-3"></div>
		<div class="col-md-5">
		<h3>관리자 로그인</h3>
		<form role="form" class="form-horizontal" action="AdminLogin.do" name="frm" method="post">
			<div class="form-group">   
		        <label for="Name"  class="col-xs-2 col-lg-2 control-label">아이디</label>
		        <div class="col-xs-10 col-lg-10">
		            <input type="text" class="form-control" placeholder="아이디" name="m_id" required="required" autofocus="autofocus"> 
		        </div>
		    </div>
		    <div class="form-group">  
		        <label for="Passward"  class="col-xs-2 col-lg-2 control-label">암호</label>
	    	    <div class="col-xs-10 col-lg-10">
		            <input type="password" class="form-control" placeholder="암호" name="m_pass" required="required" > 
		        </div> 
	    	</div> 
	  		 <div class="form-group"> 
	  		 	<div class="row"> 
		  		 	<div class="col-md-2"></div>
		  		 	<div class="col-md-8">   
			        	<button type="submit"  class="btn btn-default"> 로그인</button>   
			        	<button type="button"  class="btn btn-default" onclick="history.go(-1)"> 취소</button>
		        	</div>
		        	<div class="col-md-2"></div> 
	        	</div>
	    	</div>
	    </form>
	    </div>
	    <div class="col-md-4"></div> 

</div>
</div>
</div>
</body>
</html>
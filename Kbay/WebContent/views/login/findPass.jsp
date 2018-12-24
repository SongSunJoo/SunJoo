<%@ page language="java" contentType="text/html; charset=UTF-8"
    pageEncoding="UTF-8"%>
<!DOCTYPE html>
<html>
<head>
<meta charset="UTF-8">
<meta http-equiv="X-UA-Compatible" content="IE=edge">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>경매 상품 : 믿을 수 있는 경매, Kbay</title>

<link href="../../css/bootstrap.min.css" rel="stylesheet">
<script type="text/javascript" src="../../js/jquery.js"></script>
<script src="../../js/bootstrap.min.js"></script>
</head>
<body>
<div class="container">
	<h3 align="center">비밀번호 찾기</h3>
	<form  action="findPass.do" role="form" >  
	    <div class="form-group">   
	        <label for="Name"  class="col-xs-4 col-lg-4 control-label">아이디</label>
	        <div class="col-xs-8 col-lg-8">
	            <input type="text" class="form-control" name="id" required="required" autofocus="autofocus"> 
	        </div>
	    </div> 
	    <p>
	    <div class="form-group"> 
	    	<label for="Name"  class="col-xs-4 col-lg-4 control-label"></label>
	    	<div class="col-xs-8 col-lg-8">
        		<button type="submit"  class="btn btn-default"> 암호 찾기</button>
        	</div>     
        </div>
    </form> 

</div>
</body>
</html>
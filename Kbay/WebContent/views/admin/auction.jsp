<%@ page language="java" contentType="text/html; charset=UTF-8"
    pageEncoding="UTF-8"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
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

</head>
<body>
<div class="container">
	<div id="content">
		
			<h4>경매마감 처리결과</h4>
			<div class="col-sm-4"></div>
			<div class="col-sm-4">
				<div class="panel panel-primary">
					<div class="panel-heading">
						<h3 class="panel-title">경매마감 대상 상품 수 : ${list_total }개</h3>
					</div>
					<div class="panel-body">총 경매처리 완료 상품 수 : ${auct_total }개</div>
				</div>
			</div>
	</div>
</div>
</body>
</html>
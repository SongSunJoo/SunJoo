<%@ page language="java" contentType="text/html; charset=UTF-8"
    pageEncoding="UTF-8"%>
<%@ taglib prefix="deco" uri="http://www.opensymphony.com/sitemesh/decorator" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%-- <%@ include file="adminSession.jsp" %> --%>
<!DOCTYPE html>
<html>
<head>
<meta charset="UTF-8">
<meta http-equiv="X-UA-Compatible" content="IE=edge">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title><deco:title/></title>
<link href="../../css/bootstrap.min.css" rel="stylesheet">
<script type="text/javascript" src="../../js/jquery.js"></script>
<script src="../../js/bootstrap.min.js"></script>
<style type="text/css">
	h2 {
		text-align: center;
		color: tomato;
	}
	#footer {
		text-align: center;
	}
</style>	
<style media="screen">
	* {
		margin: 0;
		padding: 0;
	}
	
	header {
		min-width: 300px;
		height: 80px;
		background-color: #F2CB61;
		font-size: 30px;
		color: #1A4715;
		position: relative;
	}
	
	header p {
		position: absolute; /* top: 50%; left: 50%; */
		transform: translateX(-50%) translateY(-50%);
	}
	
	#dm_ul {
		min-width: 300px;
		background-color: #F2CB61;
		text-align: center;
		z-index: 3;
	}
	
	.dropmenu ul ul {
		position: absolute;
		display: none;
		z-index: 3;
	}
	
	.dropmenu ul ul li {
		display: block;
		background-color: #F2CB61;
		color: black;
		z-index: 3;
	}
	
	.dropmenu ul li {
		display: inline-block;
		margin-left: -5.5px;
		z-index: 3;
	}
	
	.dropmenu ul li a {
		display: block;
		width: 150px;
		color: black;
		line-height: 43px;
		text-decoration: none;
		z-index: 3;
	}
	
	.dropmenu ul li a:hover {
		background-color: #F2CB61;
		color: #980000;
		z-index: 3;
	}
</style>
<script type="text/javascript" src="view/js/jquery.js"></script>
<script src="https://code.jquery.com/jquery-3.2.1.min.js"></script>
<deco:head />
</head>
<body>
<%
	String m_id = request.getParameter("m_id");
%>
<a href="../admin/AdminMain.do"><img alt="로고" src="admin_images/admin_kbaylogo.jpg"></a>
<c:if test="${ m_id == null || m_id.equals('')}">
	<a href="../admin/AdminLoginForm.do" class="btn btn-default">Login</a>
	<a href="../main/Main.do" class="btn btn-default">Kbay홈</a>
</c:if>
<c:if test="${ m_id != null }">
	<a href="../admin/AdminLogout.do" class="btn btn-default">Logout</a>
	<h2>Admin Page</h2>
	<div class="dropmenu">
		<ul id="dm_ul">
	<li><a href="../main/Main.do">Kbay 메인</a>
	<li><a href="../admin/AdminProductListView.do?class_id1=A&class_id2=A1">Product</a>
		<ul>
			<li><a href="../admin/AdminProductListView.do?class_id1=A&class_id2=A1">여성의류</a></li>
			<li><a href="../admin/AdminProductListView.do?class_id1=A&class_id2=A2">남성의류</a></li>
			<li><a href="../admin/AdminProductListView.do?class_id1=A&class_id2=A3">액세서리</a></li>
			<li><a href="../admin/AdminProductListView.do?class_id1=B&class_id2=B1">스마트홈</a></li>
			<li><a href="../admin/AdminProductListView.do?class_id1=B&class_id2=B2">컴퓨터</a></li>
			<li><a href="../admin/AdminProductListView.do?class_id1=B&class_id2=B3">핸드폰</a></li>
			<li><a href="../admin/AdminProductListView.do?class_id1=C&class_id2=C1">피트니스</a></li>
			<li><a href="../admin/AdminProductListView.do?class_id1=C&class_id2=C2">캠핑용품</a></li>
			<li><a href="../admin/AdminProductListView.do?class_id1=D&class_id2=D1">코스매틱</a></li>
			<li><a href="../admin/AdminProductListView.do?class_id1=D&class_id2=D2">건강식품</a></li>
			<li><a href="../admin/AdminProductListView.do?class_id1=E&class_id2=E1">피규어</a></li>
			<li><a href="../admin/AdminProductListView.do?class_id1=E&class_id2=E2">빈티지</a></li>
		</ul>
	</li>
	<li><a href="../admin/auctionForm.do">경매마감처리</a></li>
	<li><a href="../admin/AdminKnoticeList.do">Notice</a></li>
	<li><a href="../board/B_list.do">Q&A</a></li>
</ul>
</div>
</c:if>
<script type="text/javascript">
		$(".dropmenu ul li").hover(function() {
			$(this).find("ul").stop().fadeToggle(300);
		});
</script>
<hr>
<div id="body" align="center">
<deco:body/>
</div>
<div id="footer">
<hr>
	<p>서울 강남구 테헤란로 7길 Kbay Tel. 02-1234-5678 <br>
	Copyright ⓒ 2018 by Kbay, Design by Kbay
</div>
</body>
</html>
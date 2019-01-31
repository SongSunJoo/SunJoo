<%@ page language="java" contentType="text/html; charset=UTF-8"
    pageEncoding="UTF-8" import="java.util.*" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@ taglib prefix="fmt" uri="http://java.sun.com/jsp/jstl/fmt" %>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags" %>
<%@ taglib prefix="form" uri="http://www.springframework.org/tags/form" %>
<meta http-equiv="X-UA-Compatible" content="IE=edge">
<meta name="viewport" content="width=device-width, initial-scale=1">
<!-- path = /프로젝트명 -->
<c:set var="path" value="${pageContext.request.contextPath}"></c:set>
<link href="${path}/css/bootstrap.min.css" rel="stylesheet">
<script src="${path}/js/jquery.js"></script>
<script src="${path}/js/bootstrap.min.js"></script>
<style>
 body {
		background-color: #4AABC2;
	}
	.uppermain {
		/* width: 80%; */
		margin: 30px 30px;
		background-color: #7DC9DB;
	}
	.lowerleft {
		margin: 30px 30px;
		float: left;
	}
	.lowerright {
		margin: 30px 30px;
	}
	
 .err { color: red; font-weight: bold; }
 .navigation {
 	
 }
 .navi  { 
 	float: left;
 }
 
 .ul_icon {
 	float: left;
 }
 
 .ani_tab {
 	width: 100%;
 	margin: 30px 30px;
 	cellpadding: 20px;
 }
 
 .tour_tab {
 	width: 100%;
 	margin: 20px 20px;
 	cellpadding: 10px;
 }
 
 .ani_img {
 	width: 50px;
 }
 
 .tour_img {
 	width: 50px;
 }
 </style>
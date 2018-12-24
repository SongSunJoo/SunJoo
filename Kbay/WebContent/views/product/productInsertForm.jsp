<%@page import="java.text.SimpleDateFormat"%>
<%@ page language="java" contentType="text/html; charset=UTF-8"
    pageEncoding="UTF-8" import="java.util.*"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@ taglib prefix="fmt" uri="http://java.sun.com/jsp/jstl/fmt" %>
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

<%  Date date = new Date();
	Date max_date = new Date();
	max_date.setDate(max_date.getDate()+21);
	SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd"); 
%>
<c:set var="date" value="<%=sdf.format(date) %>" />
<c:set var="max_date" value="<%=sdf.format(max_date) %>" />

<title>Insert title here</title>
	<script type="text/javascript">
	$(function() {
		$('#btn_insert').click(function() {
		/* $('form').on('submit', function() {	 */
			if ($('#p_name').val() == null || $('#p_name').val() == "") {
				alert("상품명을 입력하세요");
				frm.p_name.focus();
				return false;
			}
			
			if ($('#p_image').val() == null || $('#p_image').val() == "") {
				alert("상품 이미지를 선택하세요");
				frm.p_image.focus();
				return false;
			} 
			
			if ($('#p_price').val() == null || $('#p_price').val() == "") {
				alert("초기가격을 입력하세요");
				frm.p_price.focus();
				return false;
			}
			
			if ($('#p_price').val()%1000 != 0) {
				alert("초기가격은 반드시 1000원 단위로 입력하세요");
				frm.p_price.focus();
				return false;
			}
			
			if ($('#p_end_date').val() == null || $('#p_end_date').val() == "") {
				alert("경매마감일을 선택하세요");
				return false;
			}
			
			if ($('#p_end_date').val() < $('#p_start_date').val()) {
				alert("경매마감일은  경매시작일 이후 날짜를 선택하세요");
				$('#p_end_date').val("");
				return false;
			}
						
			$('#frm').attr('action', '../product/productInsert.do').submit();
		});
		
		/* $('#btn_cancle').click(function() {
			$('#frm').attr('action', '../product/myBidding.do').submit();
		}); */
	});
	
	</script>
</head>
<body>
	<div class="container"> 
	<div id="content" align="center">
	<div class="row">

	<!-- md : pc 크기의 컬럼을 2개 차지 -->
	<div class="col-md-2"></div>
	<div class="col-md-8">
	<h3>상품입력</h3>
	<form action="" id="frm" role="form" class="form-horizontal" method="post" enctype="multipart/form-data">
	<!-- <form action="productInsert.do" name="frm" method="post"> -->
		<input type="hidden" name="gubun" value="${gubun }">
		<input type="hidden" name="p_num" value="${p_num }">
		<input type="hidden" name="pageNum" value="${pageNum }">
		<div class="form-group">   
	        <label for="id"  class="col-xs-2 col-lg-2 control-label">상품분류</label>
	        <div class="col-xs-10 col-lg-10">
		       	 <select name="class" id="class" class="form-control">
						<optgroup label="Fashion">
							<option value="A1">여성의류</option>
							<option value="A2">남성의류</option>
							<option value="A3">액세서리</option>
						</optgroup>
						<optgroup label="Electric">
							<option value="B1">스마트홈</option>
							<option value="B2">컴퓨터</option>
							<option value="B3">핸드폰</option>
						</optgroup>
						<optgroup label="Sports">
							<option value="C1">피트니스</option>
							<option value="C2">캠핑용품</option>
						</optgroup>
						<optgroup label="Health&Beauty">
							<option value="D1">코스매틱</option>
							<option value="D2">건강식품</option>
						</optgroup>
						<optgroup label="Collection">
							<option value="E1">피규어</option>
							<option value="E2">빈티지</option>
						</optgroup>
				</select>
	        </div>
	    </div>
		<div class="form-group">   
	        <label for="p_name"  class="col-xs-2 col-lg-2 control-label">상품명</label>
	        <div class="col-xs-10 col-lg-10">
	            <input type="text" class="form-control" placeholder="상품명" id="p_name" name="p_name" required="required"> 
	        </div>
	    </div> 
		<div class="form-group">   
	        <label for="p_image"  class="col-xs-2 col-lg-2 control-label">이미지</label>
	        <div class="col-xs-10 col-lg-10">
	            <input type="file" class="form-control" placeholder="이미지" id="p_image" name="p_image" required="required"> 
	        </div>
	    </div>
		<div class="form-group">   
	        <label for="p_price"  class="col-xs-2 col-lg-2 control-label">초기가격</label>
	        <div class="col-xs-10 col-lg-10">
	            <input type="number" class="form-control" placeholder="초기가격" id="p_price" name="p_price" required="required"> 
	        </div>
	    </div>
	    <div class="form-group">   
	        <label for="p_used"  class="col-xs-2 col-lg-2 control-label">사용여부</label>
	        <div class="col-xs-10 col-lg-10" align="left">
	            <label class="radio-inline">
                <input type="radio" name="p_used" id="p_used"  value="신제품"  checked="checked">신제품
	            </label> 
	            <label class="radio-inline">
	               <input type="radio" name="p_used" id="p_used" value="중고" >중고
	            </label> 
	        </div>
	    </div>
		<div class="form-group">   
	        <label for="p_start_date"  class="col-xs-2 col-lg-2 control-label">경매시작일</label>
	        <div class="col-xs-10 col-lg-10">
	            <input type="date" class="form-control" placeholder="경매시작일" id="p_start_date" name="p_start_date" value="${date }" readonly="readonly"> 
	        </div>
	    </div>
		<div class="form-group">   
	        <label for="p_end_date"  class="col-xs-2 col-lg-2 control-label">경매마감일</label>
	        <div class="col-xs-10 col-lg-10">
	            <input type="date" class="form-control" placeholder="경매마감일" id="p_end_date" name="p_end_date"  max="${max_date }"> 
	        </div>
	    </div>
		<div class="form-group"> 
  		 	<div class="row"> 
	  		 	<div class="col-md-2"></div>
	  		 	<div class="col-md-8">   
		        	<button type="submit" id="btn_insert" class="btn btn-default"> 입력</button>   
		        	<button type="submit"  id="btn_cancle" class="btn btn-default" onclick="history.go(-1)"> 취소</button>
	        	</div>
	        	<div class="col-md-2"></div> 
        	</div>
    	</div>
	<%-- 
		<table>
			<tr><th>상품분류</th>
				<td><!-- <label for="class">상품분류</label>  -->
					<select name="class"  class="form-control">
						<optgroup label="Fashion">
							<option value="A1">여성의류</option>
							<option value="A2">남성의류</option>
							<option value="A3">액세서리</option>
						</optgroup>
						<optgroup label="Electric">
							<option value="B1">스마트홈</option>
							<option value="B2">컴퓨터</option>
							<option value="B3">핸드폰</option>
						</optgroup>
						<optgroup label="Sports">
							<option value="C1">피트니스</option>
							<option value="C2">캠핑용품</option>
						</optgroup>
						<optgroup label="Health&Beauty">
							<option value="D1">코스매틱</option>
							<option value="D2">건강식품</option>
						</optgroup>
						<optgroup label="Collection">
							<option value="E1">피규어</option>
							<option value="E2">빈티지</option>
						</optgroup>
				</select>
				</td></tr>
			<tr><th>상품명</th>
				<td><input type="text" name="p_name" required="required" autofocus="autofocus"></td></tr>
			<tr><th>이미지</th>
				<td><input type="file" name="p_image" required="required" ></td></tr>
			<tr><th>초기가격</th>
				<td><input type="number" name="p_price" required="required"></td></tr>	
			<tr><th>사용여부</th>
				<td><label for="p1">신제품</label><input type="radio" name="p_used" value="신제품" id="p1" checked="checked">
					<label for="p2">중고</label><input type="radio" name="p_used" value="중고" id="p2"></td></tr>
			<tr><th>경매시작일</th>
				<td><input type="text" name="p_start_date" 
				value='<fmt:formatDate value="${date }" pattern="yyyy-MM-dd" />' readonly="readonly"></td></tr>
				value="${date }" readonly="readonly"></td></tr>
			<tr><th>경매마감일</th>
				<td><input type="date" name="p_end_date" required="required" max="${max_date }"></td></tr>
			<tr><th colspan="2"><input type="submit" value="확인"  onclick="Chk()">&nbsp;
					<input type="submit" value="취소"  onclick="re_turn()"></th></tr>				
		</table> --%>
		
		<h5>※ 초기가격은 반드시 1000원 단위로 입력하세요.</h5>
		<h5>※ 경매마감일은 경매시작일 기준 최대 3주(21일)입니다.</h5>
		</form>
	</div>
	<div class="col-md-2"></div>
	</div>
	</div>
	</div>

</body>
</html>
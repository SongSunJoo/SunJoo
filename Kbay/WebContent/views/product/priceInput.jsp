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
<script type="text/javascript">
	$(function() {
		$('#btn_ok').click(function() {
		/* $('form').on('submit', function() {	 */
			if ($('#newPrice').val() == null || $('#newPrice').val() == "") {
				alert("Bidding 가격을 입력하세요");
				frm.newPrice.focus();
				return false;
			}
			
			/* if ($('#newPrice').val() < "${max_price}") { */
			if (($('#newPrice').val()-"${max_price}") <= 0) {
				/* alert("현재 가격보다 더 큰 금액을 입력하세요 : " + $('#newPrice').val() + ", ${max_price}, " + ($('#newPrice').val()-"${max_price}")); */
				alert("현재 가격보다 더 큰 금액을 입력하세요 ");
				frm.newPrice.focus();
				return false;
			} 
			
			if ($('#newPrice').val()%1000 != 0) {
				alert("가격은 반드시 1000원 단위로 입력하세요");
				frm.newPrice.focus();
				return false;
			} 
			
			$('#frm').attr('action', '../product/priceInsert.do').submit();
		});
		
		$('#btn_cancle').click(function() {
			$('#frm').attr('action', '../product/productDetail.do').submit();
		});
	});

	</script>

</head>
<body>
<div class="container">
	<div class="col-md-4"></div>
	<div class="col-md-4" align="center">
		<div class="row" align="center">
		<form name="frm" id="frm">
			<input type="hidden" name="a_id" value="${id }">
			<input type="hidden" name="p_num" value="${p_num }">
			<input type="hidden" name="gubun" value="${gubun }">
			<input type="hidden" name="pageNum" value="${pageNum }">
				<h4>상품 현재가격 : ${max_price }원</h4><br>
				Budding 가격 : <input type="number" id="newPrice" name="newPrice" required="required" autofocus="autofocus"><p><p>
				<h6>※ 가격은 반드시 1000원 단위로 입력하세요.</h6><p>　</p>
				<input type="submit" class="btn btn-default" value="확인" id="btn_ok">
			 	<input type="submit" class="btn btn-default" value="취소" id="btn_cancle" >
				
		 </form></div>
	 </div>
	<div class="col-md-4"></div>
</div>
</body>
</html>

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
	date.setDate(date.getDate());
	SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd"); 
	%>

<c:set var="date" value="<%=sdf.format(date) %>" />

<script type="text/javascript">
$(function() {
	$('#btn_bidding').click(function() {
		if ("${pInfo.p_end_date }" < "${date}") {
			alert("경매가 종료된 상품입니다.");
			return;
		}
		
		$('#frm').attr('action', '../product/priceInput.do').submit();
	});
	
	$('#btn_history').click(function() {
		$('#frm').attr('action', '../product/priceHistory.do').submit();
	});
	
	$('#btn_list').click(function() {
		$('#frm').attr('action', '../product/myBidding.do').submit();
	});
});

</script>
<style type="text/css">
	ul li {
		list-style: none;
	}
</style>
</head>

<body>
	<div class="container">
		<div id="content" align="center">
			<h3>상품 상세정보</h3>
			<form action="" id="frm">
				<input type="hidden" name="id" value="${pInfo.id }">
				<input type="hidden" name="gubun" value="${gubun}">
				<input type="hidden" name="p_num" value="${pInfo.p_num }">
				<input type="hidden" name="p_name" value="${pInfo.p_name }">
				<input type="hidden" name="p_class_name1" value="${pInfo.p_class_name1 }">
				<input type="hidden" name="p_class_name2" value="${pInfo.p_class_name2 }">
				<input type="hidden" name="pageNum" value="${pageNum }">
				<input type="hidden" name="p_image" value="${pInfo.p_image }">
				<table class="table table-hover">
					<tr>
						<td align="center" rowspan="8">
							<div class="thumbnail">
								<img alt="${pInfo.p_name }" src="../../fileSave/${pInfo.p_image }" width="300" height="300">
							</div>
						</td>
						<th>대   분   류</th><td>${pInfo.p_class_name1 }</td></tr>
						<tr><th>소   분   류</th><td>${pInfo.p_class_name2 }</td></tr>
						<tr><th>상   품   명</th><td>${pInfo.p_name }</td></tr>
						<tr><th>사 용 여 부</th><td>${pInfo.p_used }</td></tr>
						<tr><th>경매시작일</th><td>${pInfo.p_start_date }</td></tr>
						<tr><th>경매종료일</th><td>${pInfo.p_end_date }</td></tr>
						<tr><th>경 매 결 과</th><td>${pInfo.p_auct_desc }</td></tr>
						<tr><th>가         격</th><td>${pInfo.p_price }원</td></tr>
				</table>
				<%-- 
				
				<table>
					<tr>
						<td align="center">
							<div class="thumbnail">
								<img alt="${pInfo.p_name }" src="../../fileSave/${pInfo.p_image }" width="300" height="300">
							</div>		
						</td>
						<td>
							<ul>
								<li>상품명 : ${pInfo.p_name }</li>
								<li>대분류 : ${pInfo.p_class_name1 }</li>
								<li>소분류 : ${pInfo.p_class_name2 }</li>
								<li>사용여부 : ${pInfo.p_used }</li>
								<li>경매시작일 : ${pInfo.p_start_date }</li>
								<li>경매종료일 : ${pInfo.p_end_date }</li>
								<li>경매결과 : ${pInfo.p_auct_desc }</li>
								<li>가격 : ${pInfo.p_price }원</li>
							</ul>
						</td>
					</tr>
				</table> --%>
				<input type="submit" class="btn btn-default" id="btn_bidding" value="Bidding참여">&nbsp;
				<input type="submit" class="btn btn-default" id="btn_history" value="가격 History">&nbsp;
				<input type="submit" class="btn btn-default" id="btn_list" value="목록보기">
				
			</form>
		</div>
	</div>
</body>
</html>
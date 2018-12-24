<%@ page language="java" contentType="text/html; charset=UTF-8"
    pageEncoding="UTF-8"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<!DOCTYPE html>
<html>
<head>
<meta charset="UTF-8">
<meta http-equiv="X-UA-Compatible" content="IE=edge">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>경매 가격 정보 : 믿을 수 있는 경매, Kbay</title>
<link href="../../css/bootstrap.min.css" rel="stylesheet">
<script type="text/javascript" src="../../js/jquery.js"></script>
<script src="../../js/bootstrap.min.js"></script>
<script type="text/javascript">
$(function() {
	$('#btn_return').click(function() {
		
		$('#frm').attr('action', 'ProductListDetail.do').submit();
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
			<div class="row">
			<h3>상품 상세정보</h3>
			<form action="" id="frm">
				<input type="hidden" name="p_num" value="${p_num }">
				<input type="hidden" name="pageNum" value="${pageNum }">
				<table>
					<tr><td>
							<table class="table">
								<tr><td align="center">
									
									<div class="thumbnail">
										<img alt="${p_image }" src="../../fileSave/${p_image }" width="300" height="300">
									</div>
									</td>
									<td>
										<div class="panel panel-default">
											<div class="panel-heading"><h4 class="panel-title">대분류&nbsp;&nbsp;&nbsp;${p_class_name1 }</h4> </div>
											<div class="panel-heading"><h4 class="panel-title">소분류&nbsp;&nbsp;&nbsp;${p_class_name2 }</h4> </div>
											<div class="panel-heading"><h4 class="panel-title">상품명&nbsp;&nbsp;&nbsp;${p_name }</h4> </div>
										</div>
										<table class="table table-hover">
											<h5 style="font-weight: bold" align="center">가격 History</h5>
											<tr><th>등록일자</th><th>가격</th></tr>
											
											<c:if test="${empty list }">
												<tr><td colspan="2">등록된 경매 가격이 없습니다</td></tr>
											</c:if>
											
											<c:if test="${not empty list }">
												<c:forEach var="pInfo" items="${list }">
													<tr><td>${pInfo.a_reg_date }</td><td>${pInfo.a_price }</td></tr>
												</c:forEach>
											</c:if>
										</table>
										<p>
										<input type="submit" class="btn btn-default" id="btn_return" value="돌아가기" >
									</td>
								</tr>
							</table>
						</td>
					</tr>
				</table>
			</form>
		</div>
		</div>
	</div>
</body>
</html>
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
<script src="../../js/jquery.js"></script>
<script src="../../js/bootstrap.min.js"></script>
<style type="text/css">
	td {
		color: red;
	}
</style>
</head>
<body>
<table class="table"><thead>
<c:if test="${empty newItem }">
	<tr class="danger">
		<td align="center">등록된 New Item이 존재하지 않습니다.</td></tr>
</c:if>
<c:if test="${not empty newItem }">
	<tr>
	<c:forEach var="product" items="${newItem }">
		<td>
			<div class="row">
				<div class="col-md-12">
					<div class="thumbnail"><a href="../productlist/ProductListDetail.do?p_num=${product.p_num }&pageNum=1"><img src="../../fileSave/${product.p_image }" alt="${product.p_image }" width="132" height="132"></a><p>
						<div class="caption"><a href="../productlist/ProductListDetail.do?p_num=${product.p_num }&pageNum=1">${product.p_name}</a><p>
						초기가격 : ${product.p_price }원<br>
						사용여부 : ${product.p_used }<br>
						등록일 : ${product.p_start_date }<br>
						</div>
					</div>
				</div>
			</div>
		</td>
	</c:forEach>
	</tr>
</c:if>
</table>
<br><br><br><br>
</body>
</html>
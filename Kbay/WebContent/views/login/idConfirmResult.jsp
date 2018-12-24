<%@ page language="java" contentType="text/html; charset=UTF-8"
    pageEncoding="UTF-8" import="Kbay.dao.*"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<!DOCTYPE html>
<html>
<head>
<meta charset="UTF-8">
<title>Insert title here</title>
<!-- <script type="text/javascript">
	function cl() {
		opener.frm.id.value = "${id}";
		window.close();
	}
</script> -->
</head>
<body>
<!-- id가 있을 때, 결과값은 1 -->
 <c:if test="${result>0}">
		이미 있는 아이디입니다
		다른 아이디를 입력하세요
<!-- 		<form>
			<table>
				<tr><th>아이디</th>
					<td><input type="text" name="id" required="required"></td></tr>
				<tr><th colspan="2"><input type="submit" value="확인"></th></tr>
			</table>
		</form> -->
	</c:if>
<!-- id가 있을 때, 결과값은 1 -->
	<c:if test="${result <= 0}">
		사용 가능한 아이디 입니다
		
	</c:if>
</body>
</html>
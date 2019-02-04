<%@ page language="java" contentType="text/html; charset=UTF-8"
	pageEncoding="UTF-8"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<!DOCTYPE html>
<html>
<head>
<meta charset="UTF-8">
<title>공지사항 작성</title>
</head>
<body>

	<h3 align="center">공지사항 작성</h3>
	<hr>
	<form action="AdminKnoticeInsert.do" method="post">
		<input type="hidden" name="n_num" value="${n_num }"> 
		<input type="hidden" name="pageNum" value="${pageNum }">
		
		<table>
			<tr>
				<th>작성자</th>
				<td><input type="text" name="m_id" id="m_id" value="${m_id }"></td>
			</tr>
			<tr>
				<th>제목</th>
				<td><input type="text" name="n_subject" required="required"></td>
			</tr>
			<tr>
				<th>내용</th>
				<td><textarea rows="10" cols="30" name="n_content"
						required="required"></textarea></td>
			</tr>
			<tr>
				<th colspan="2"><input type="submit" value="확인"></th>
			</tr>
		</table>
	</form>

</body>
</html>
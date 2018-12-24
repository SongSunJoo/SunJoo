<%@ page language="java" contentType="text/html; charset=UTF-8"
    pageEncoding="UTF-8"%>
<!DOCTYPE html>
<html>
<head>
<meta charset="UTF-8">
<meta http-equiv="X-UA-Compatible" content="IE=edge">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title><deco:title></deco:title></title>
<link href="../../css/bootstrap.min.css" rel="stylesheet">
<script src="../../js/jquery.js"></script>
<script src="../../js/bootstrap.min.js"></script>

</head>
<body>
<script type="text/javascript" src="../../js/jquery.js"></script>
<script type="text/javascript">
	$(function() {
		/* setInterval(() => {
			$('#ani').toggle({
				left: 100
			}, 'slow');
		}, 1000); */
		
		$('#btn').click(function() {
			$('#frm').attr('action', 'auction.do').submit();
		});
	});
</script>
<div class="container">
	<div id="content" align="center">
		<div class="row">
			<div class="col-md-4"></div>
			<div class="col-md-4">
				<form id="frm" class="form-group">
					<div class="row">
						<div class="col-md-8">
							<div id="ani">경매마감 처리를 진행하고 있습니다</div>
						</div>
						<div class="col-md-4">
							<button type="submit" id="btn"  class="btn btn-default"> 확인</button>
						</div>
					</div>
				</form>
				<div class="progress progress-striped active">
					<div class="progress-bar" role="progressbar" aria-valuenow="65" aria-valuemin="0" aria-valuemax="100" style="width: 45%">
						<span class="sr-only"></span> </div>
				</div>
			</div>
			<div class="col-md-4"></div>
		</div>
	</div>
</div>

</body>
</html>




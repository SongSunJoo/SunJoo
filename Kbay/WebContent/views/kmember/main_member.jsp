<%@ page language="java" contentType="text/html; charset=UTF-8"
    pageEncoding="UTF-8"%>
<!DOCTYPE html>
<html>
<head>
<meta charset="UTF-8">
<meta http-equiv="X-UA-Compatible" content="IE=edge">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>합리적인 경매, Kbay입니다</title>
<link href="../../css/bootstrap.min.css" rel="stylesheet">
<script src="../../js/jquery.js"></script>
<script src="../../js/bootstrap.min.js"></script>
<style type="text/css">
	/* Animation Canvas */
	.animation_canvas {
		overflow: hidden;
		position: relative;
		width: 754px;
		height: 400px;
		z-index: 1;
	}
	/* Slider Panel */
	.slider_panel {
		width: 3770px;
		position: relative;
	}
	
	.slider_image {
		float: left;
		width: 754px;
		height: 400px;
		margin: 10px auto;
		padding: 0;
		z-index: 1;
	}
 	
	/* Control Panel */
	.control_panel {
		position: absolute;
		top: 380px;
		left: 270px;
		height: 13px;
		overflow: hidden;
	}
	
	.control_button {
		width: 12px;
		height: 46px;
		position: relative;
		float: left;
		cursor: pointer;
		background: url('../../images/point_button.png');
	}
	
	.control_button:hover {
		top: -16px;
	}
	
	.control_button.active {
		top: -31px;
	}
	
	.slider_text h1 {
		color: white;
	}
	
	h2 {
		color: #005000;
		padding: 12px;
	}
</style>
</head>
<body>
<script src="../js/jquery.js"></script>
<script>
	$(document).ready(
			function() {
				// 슬라이더를 움직여주는 함수
				function moveSlider(index) {
					// 슬라이더를 이동합니다.
					var willMoveLeft = -(index * 754); // -는 왼쪽으로 옮김
					$('.slider_panel').animate({
						left : willMoveLeft
					}, 'slow');
					// control_button에 active클래스를 부여/제거합니다.
					$('.control_button[data-index=' + index + ']').addClass(
							'active');
					$('.control_button[data-index!=' + index + ']')
							.removeClass('active');
					// 글자를 이동합니다.
					$('.slider_text[data-index=' + index + ']').show().animate(
							{
								left : 0
							}, 'slow');
					$('.slider_text[data-index!=' + index + ']').hide('slow',
							function() {
								$(this).css('left', -300);
							});
				}
				// 초기 텍스트 위치 지정 및 data-index 할당
				$('.slider_text').css('left', -300).each(function(index) {
					$(this).attr('data-index', index);
				});

				// 컨트롤 버튼의 클릭 핸들러 지정 및 data-index 할당
				$('.control_button').each(function(index) {
					$(this).attr('data-index', index);
				}).click(function() {
					var index = $(this).attr('data-index');
					moveSlider(index);
				});
				// 초기 슬라이더 위치 지정
				var randomNumber = Math.floor(Math.random() * 5);
				moveSlider(randomNumber);
			/* 	$('slider_panel').css('width', 754);
				setInterval(function() {
					$('body').append($('slider_panel').first());
				}, 2000); */
			});
	
	$(function() {
		$('#bidding').load('../main/hotBidding.do');
		$('#item').load('../main/newItem.do');
	});
</script>
<div class="container">
		<div id="content" align="center">
			<div class="row">
				<h2>Event</h2>
					<div class="animation_canvas">
						<div class="slider_panel">
							<img src="../../images/fashion.jpg" class="slider_image" /> 
							<img src="../../images/computer.jpg" class="slider_image" /> 
							<img src="../../images/sports.jpg" class="slider_image" /> 
							<img src="../../images/cosmetic.jpg" class="slider_image" /> 
							<img src="../../images/collection.jpg" class="slider_image" />
						</div>
						<div class="control_panel">
							<div class="control_button"></div>
							<div class="control_button"></div>
							<div class="control_button"></div>
							<div class="control_button"></div>
							<div class="control_button"></div>
						</div>
					</div>
			</div>
		</div>
	</div>
	<br><br><br>
	<div class="container">
		<div id="content" align="center">
			<div class="row">
				<h2>Hot Bidding</h2>
					<div id="hotBidding">
						<div id="bidding"></div><p>
					</div>
			</div>
		</div>
	</div>
	<p>
	<p>
	<div class="container">
		<div id="content" align="center">
			<div class="row">
				<h2>New Item</h2>
					<div id="newProduct">
						<div id="item"></div><p>
					</div>
			</div>
		</div>
	</div>
	<p>
	<p>
</body>
</html>
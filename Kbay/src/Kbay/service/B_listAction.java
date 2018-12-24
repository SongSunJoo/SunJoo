package Kbay.service;
import java.util.List;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import Kbay.dao.KboardDao;
import Kbay.model.Kboard;
public class B_listAction implements CommandProcess {
	public String requestPro(HttpServletRequest request, HttpServletResponse response) {
		// 페이지당 줄수, 페이지당 블럭수
		int rowPerPage = 10;
		int pagePerBlock = 10;
		
		// 페이지 넘버를 받아 옴.
		String pageNum = request.getParameter("pageNum");
		// 만약에 페이지 넘버가 없을 때, 페이지 넘버는 1
		// 만약 페이지 넘버가 있을 때, 페이지 번호를 클릭해서 들어오면, 해당 페이지 넘버를 받아 옴
		if (pageNum==null || pageNum.equals("")) {
			pageNum = "1";
		}
		// 받아 온 pageNum을 현재 페이지로 지정
		int currentPage = Integer.parseInt(pageNum);
		// 시작 줄(첫번째줄) = (현재 페이지 - 1) * 한페이지당줄수 + 1 (본인)
		int startRow = (currentPage - 1)*rowPerPage + 1;
		// 마지막 줄 = 시작 줄 + 한페이지당 줄수 - 1 (본인)
		int endRow = startRow + rowPerPage - 1;
		
		//dao 파일내 객체 생성
		KboardDao bd = KboardDao.getInstance();
		List<Kboard> list = bd.list(startRow, endRow);
		// dao내 total 객체 생성 = 전체 게시물 숫자
		int tot = bd.total();
		// 전체 게시물 수 - 해당 페이지에서 시작되는 줄 번호를 빼고, 1을 더해줌
		int total = tot - startRow + 1;
		//예를들어 현재 페이지가 23일 때, 스타트 페이지는 20-엔드 페이지는 29가 되도록 수식 만들어 줌
		int startPage = currentPage - (currentPage - 1) % 10;
		int endPage = startPage + pagePerBlock - 1;
		// Math.ceil 은 소수값이 존재할 때 값을 올리는 역활을 하는 함수
		// Math.floor 는 소수값이 존재할 때 소수값을 버리는 역활을 하는 함수
		// Math.round 반올림 함수
		int totPage = (int) Math.ceil((double)tot/rowPerPage);
		// 만약에 마지막 페이지가 전체페이지보다 더 클 경우,
		// 마지막 페이지 = 전체 페이지
		if (endPage > totPage) {
			endPage = totPage;
		}
				
		request.setAttribute("total", total);
		request.setAttribute("list", list);
		request.setAttribute("startPage", startPage);
		request.setAttribute("endPage", endPage);
		request.setAttribute("totPage", totPage);
		request.setAttribute("pagePerBlock", pagePerBlock);
		request.setAttribute("currentPage", currentPage);
		
		return "board/B_list";
	}
}
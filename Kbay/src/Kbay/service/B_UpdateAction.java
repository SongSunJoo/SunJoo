package Kbay.service;
import java.io.UnsupportedEncodingException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import Kbay.dao.KboardDao;
import Kbay.model.Kboard;
public class B_UpdateAction implements CommandProcess {
	public String requestPro(HttpServletRequest request, HttpServletResponse response) {
		try { request.setCharacterEncoding("utf-8");
		} catch (UnsupportedEncodingException e) {	}
		
		String pageNum = request.getParameter("pageNum");
		int b_num = Integer.parseInt(request.getParameter("b_num"));
		
		// 실제 수정한 String 값
		Kboard kboard = new Kboard();
		kboard.setB_subject(request.getParameter("b_subject"));
		kboard.setB_content(request.getParameter("b_content"));
		kboard.setB_num(b_num);
		
		// 업데이트가 성공 실패했는지 여부만 표시 if int = 1이면 성공, for jsp 파일용
		KboardDao bd = KboardDao.getInstance();
		int result = bd.update(kboard);
		
		request.setAttribute("pageNum", pageNum);
		request.setAttribute("kboard", kboard);
		request.setAttribute("result", result);
		
		System.out.println("Update b_num : " + b_num);
		System.out.println("result : " + result);
		return "board/B_updateResult";
	}
	

}

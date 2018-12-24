package Kbay.service;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import Kbay.dao.KboardDao;
import Kbay.model.Kboard;
public class B_InsertForm implements CommandProcess {
	public String requestPro(HttpServletRequest request, HttpServletResponse response) {
		HttpSession session = request.getSession();
		String id = (String) session.getAttribute("id");
		
		if (id == null || id.equals(""))
			return "login/loginForm";
		
		int b_num = 0, b_ref = 0, b_re_level = 0, b_re_step = 0;
		
		String pageNum = request.getParameter("pageNum");
		System.out.println("pageNum = " + pageNum);
		
		String num = request.getParameter("b_num");
		System.out.println("b_num = "+ num);
		
		if (num != null) { // 만약 b_num이 있으면, 답변글
			b_num = Integer.parseInt(num);
			KboardDao bd = KboardDao.getInstance();
			// board 읽은 글
			Kboard board = bd.select(b_num);
			b_ref = board.getB_ref();
			b_re_step  = board.getB_re_step();
			b_re_level = board.getB_re_level();
		}	
		
		
		/*if (pageNum == null || pageNum.equals("")) {
			pageNum = "1";
		}
		
		if (b_num == null || b_num.equals("")) {
			b_num = "1";
		} else {
			num = Integer.parseInt(b_num);
		}*/
		
		request.setAttribute("id", id);
		request.setAttribute("pageNum", pageNum);
		request.setAttribute("b_num", b_num);
		request.setAttribute("b_ref", b_ref);
		request.setAttribute("b_re_level", b_re_level);
		request.setAttribute("b_re_step", b_re_step);
		
		
		System.out.println(" InsertForm pageNum : " + pageNum);
		System.out.println(" InsertForm b_num : " + b_num);
		System.out.println(" InsertForm b_ref : " + b_ref);
		System.out.println(" InsertForm num : " + num);
		System.out.println(" InsertForm b_re_level : " + b_re_level);
		System.out.println(" InsertForm b_re_step : " + b_re_step);
		
		return "board/B_insertForm";
	}
}
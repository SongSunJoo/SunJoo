package Kbay.service;
import java.io.UnsupportedEncodingException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import Kbay.dao.KboardDao;
import Kbay.model.Kboard;

public class B_InsertAction implements CommandProcess {
	public String requestPro(HttpServletRequest request, HttpServletResponse response) {
		// 한글 깨짐 방지용
		try{ request.setCharacterEncoding("utf-8");
		} catch (UnsupportedEncodingException e) {	}
		
		System.out.println("board insert action");
		
		/*HttpSession session = request.getSession();
		String id = (String) session.getAttribute("id");
		
		if (id == null || id.equals(""))
			return "login/loginForm";*/
		
		/*String id = request.getParameter("id");
		System.out.println("B_insert id = " + id);*/
		
		Kboard kboard = new Kboard();
		int b_num = Integer.parseInt(request.getParameter("b_num"));
		System.out.println("b_num = " + b_num);
		
		kboard.setB_num(Integer.parseInt(request.getParameter("b_num")));
				
		kboard.setB_subject(request.getParameter("b_subject"));
		System.out.println("b_subject = "+request.getParameter("b_subject"));
		
		kboard.setId(request.getParameter("id"));
		/*kboard.setId(id);*/
		kboard.setB_content(request.getParameter("b_content"));
		kboard.setB_pass(request.getParameter("b_pass"));
		kboard.setB_re_level(Integer.parseInt(request.getParameter("b_re_level")));
		kboard.setB_ref(Integer.parseInt(request.getParameter("b_ref")));
		kboard.setB_re_step(Integer.parseInt(request.getParameter("b_re_step")));
		
		String pageNum = request.getParameter("pageNum");
		KboardDao bd = KboardDao.getInstance();
		int result  = bd.insert(kboard);
		
		request.setAttribute("result", result);
		request.setAttribute("pageNum", pageNum);
		return "board/B_insertResult";
	}
}
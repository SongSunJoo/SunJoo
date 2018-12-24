package Kbay.service;
import java.io.UnsupportedEncodingException;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import Kbay.dao.KboardDao;
import Kbay.model.Kboard;
public class B_UpdateForm implements CommandProcess {
	public String requestPro(HttpServletRequest request, HttpServletResponse response) {
		try { request.setCharacterEncoding("utf-8");
		} catch (UnsupportedEncodingException e) {	}
		
		int num = Integer.parseInt(request.getParameter("b_num"));
		String pageNum = request.getParameter("pageNum");

		KboardDao bd = KboardDao.getInstance();
		Kboard kboard = bd.select(num);
		
		request.setAttribute("pageNum", pageNum);
		request.setAttribute("kboard", kboard);
		
		return "board/B_updateForm";
	}
	
}

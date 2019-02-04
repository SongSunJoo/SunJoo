package Kbay.service;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

public class AdminKnoticeInsertForm implements CommandProcess {

	@Override
	public String requestPro(HttpServletRequest request, HttpServletResponse response) {
		int num = 0;
		String pageNum = request.getParameter("pageNum");
		
		String n_num = request.getParameter("num");
		
		if(pageNum == null || pageNum.equals("")) {
			pageNum ="1";
		}
		
		if (n_num == null || n_num.equals("")) {
			n_num = "1";
		} else {
			num = Integer.parseInt(n_num);
		}
		
		request.setAttribute("pageNum", pageNum);
		request.setAttribute("n_num", n_num);
		
		System.out.println("AdminKnoticeInsertForm pageNum : " + pageNum);
		System.out.println("AdminKnoticeInsertForm n_num : " + n_num);
		System.out.println("AdminKnoticeInsertForm num : " + num);
		
		return "admin/adminKnoticeInsertForm";
	}

}

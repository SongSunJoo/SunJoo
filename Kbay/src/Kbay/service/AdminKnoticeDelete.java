package Kbay.service;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import Kbay.dao.KnoticeDao;

public class AdminKnoticeDelete implements CommandProcess {

	@Override
	public String requestPro(HttpServletRequest request, HttpServletResponse response) {
		int n_num = Integer.parseInt(request.getParameter("n_num"));
		String pageNum = request.getParameter("pageNum");
		
		KnoticeDao kd = KnoticeDao.getInstance();
		int result = kd.delete(n_num);
		
		request.setAttribute("result", result);
		request.setAttribute("pageNum", pageNum);
		
		return "admin/adminKnoticeDelete";
	}

}

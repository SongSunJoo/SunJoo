package Kbay.service;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import Kbay.dao.KnoticeDao;
import Kbay.model.Knotice;

public class AdminKnoticeView implements CommandProcess {

	@Override
	public String requestPro(HttpServletRequest request, HttpServletResponse response) {
		int n_num = Integer.parseInt(request.getParameter("n_num"));
		String pageNum = request.getParameter("pageNum");
		KnoticeDao kd = KnoticeDao.getInstance();
		Knotice knotice = kd.select(n_num);
		
		request.setAttribute("pageNum", pageNum);
		System.out.println("view pageNum : " + pageNum);
		request.setAttribute("knotice", knotice);
		
		return "admin/adminKnoticeView";
	}

}

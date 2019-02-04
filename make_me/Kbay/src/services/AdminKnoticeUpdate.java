package Kbay.service;

import java.io.UnsupportedEncodingException;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import Kbay.dao.KnoticeDao;
import Kbay.model.Knotice;

public class AdminKnoticeUpdate implements CommandProcess {

	@Override
	public String requestPro(HttpServletRequest request, HttpServletResponse response) {
		try {
			request.setCharacterEncoding("utf-8");
		} catch (UnsupportedEncodingException e) {	}
	
		String pageNum = request.getParameter("pageNum");
		int n_num = Integer.parseInt(request.getParameter("n_num"));
		
		Knotice knotice = new Knotice();
		knotice.setN_subject(request.getParameter("n_subject"));
		knotice.setN_content(request.getParameter("n_content"));
		knotice.setN_num(n_num);
		KnoticeDao kd = KnoticeDao.getInstance();
		int result = kd.update(knotice);
		
		request.setAttribute("pageNum", pageNum);
		request.setAttribute("knotice", knotice);
		request.setAttribute("result", result);
		
		System.out.println("AdminKnoticeUpdate pageNum : " + pageNum);
		System.out.println("AdminKnoticeUpdate knotice: " + knotice);
		System.out.println("AdminKnoticeUpdate result : " + result);
		System.out.println("AdminKnoticeUpdate n_num : " + n_num);
		
		return "admin/adminKnoticeUpdate";
	}

}

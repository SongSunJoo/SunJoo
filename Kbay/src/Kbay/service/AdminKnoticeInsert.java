package Kbay.service;

import java.io.UnsupportedEncodingException;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import Kbay.dao.KnoticeDao;
import Kbay.model.Knotice;

public class AdminKnoticeInsert implements CommandProcess {

	@Override
	public String requestPro(HttpServletRequest request, HttpServletResponse response) {
		try {
			request.setCharacterEncoding("uft-8");
		} catch (UnsupportedEncodingException e) {}
		
		Knotice knotice = new Knotice();
		knotice.setN_num(Integer.parseInt(request.getParameter("n_num")));
		knotice.setM_id(request.getParameter("m_id"));
		knotice.setN_subject(request.getParameter("n_subject"));
		knotice.setN_content(request.getParameter("n_content"));
		
		String pageNum = request.getParameter("pageNum");
		KnoticeDao kd = KnoticeDao.getInstance();
		int result = kd.insert(knotice);
		
		request.setAttribute("pageNum", pageNum);
		request.setAttribute("result", result);
		
		System.out.println("AdminKnoticeInsert pageNum : " + pageNum );
		System.out.println("AdminKnoticeInsert result : " + result);
		
		return "admin/adminKnoticeInsert";
	}
}

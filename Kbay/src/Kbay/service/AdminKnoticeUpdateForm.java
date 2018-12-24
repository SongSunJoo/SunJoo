package Kbay.service;

import java.io.UnsupportedEncodingException;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import Kbay.dao.KnoticeDao;
import Kbay.model.Knotice;

public class AdminKnoticeUpdateForm implements CommandProcess {
	@Override
	public String requestPro(HttpServletRequest request, HttpServletResponse response) {
		try {
			request.setCharacterEncoding("utf-8");
		} catch (UnsupportedEncodingException e) {	}

		
		int n_num = Integer.parseInt(request.getParameter("n_num"));
		String pageNum = request.getParameter("pageNum");
		
		KnoticeDao kd = KnoticeDao.getInstance();
		Knotice knotice = kd.select(n_num);
		
		request.setAttribute("pageNum", pageNum);
		request.setAttribute("knotice", knotice);
		
		System.out.println("AdminKnotice pageNum : " + pageNum);
		System.out.println("AdminKnotice n_num : " + n_num);
		System.out.println("AdminKnotice knotice : " + knotice);
		
		return "admin/adminKnoticeUpdateForm";
	}

}

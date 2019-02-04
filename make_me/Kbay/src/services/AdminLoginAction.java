package Kbay.service;

import java.io.UnsupportedEncodingException;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import Kbay.dao.KManagerDao;
import Kbay.model.KManager;

public class AdminLoginAction implements CommandProcess {

	@Override
	public String requestPro(HttpServletRequest request, HttpServletResponse response) {
		try {
			request.setCharacterEncoding("utf-8");
		} catch (UnsupportedEncodingException e) {}
		
		String m_id = request.getParameter("m_id");
		String m_pass = request.getParameter("m_pass");
		
		System.out.println("m_id : " + m_id);
		System.out.println("m_pass : " + m_pass);
		
		KManagerDao kd = KManagerDao.getInstance();
		KManager kmanager = kd.select(m_id);
		
		System.out.println("kmanager : " + kmanager);
		
		int result = 0;
		if (kmanager == null) {
			result = -1;
		} else {
			if(kmanager.getM_pass().equals(m_pass)) {
				HttpSession session = request.getSession();
				session.setAttribute("m_id", m_id);
				result = 1;
			} else {
				result = 0;
			}	
		}
		
		System.out.println("result : " + result);
		
		request.setAttribute("result", result);
		return "admin/adminLogin";
	}

}

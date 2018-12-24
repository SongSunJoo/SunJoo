package Kbay.service;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import Kbay.dao.KmemberDao;
import Kbay.model.Kmember;

public class FindPassAction implements CommandProcess {
	public String requestPro(HttpServletRequest request, HttpServletResponse response) {

		String id = request.getParameter("id");
		
		KmemberDao md = KmemberDao.getInstance();
		Kmember kme	= md.findPass(id);
		
		request.setAttribute("kme", kme);
		
		return "login/findPassResult";
	}
}

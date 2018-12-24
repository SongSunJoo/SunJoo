package Kbay.service;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import Kbay.dao.KmemberDao;
import Kbay.model.Kmember;

public class LoginMainAction implements CommandProcess {
	public String requestPro(HttpServletRequest request, HttpServletResponse response) {
		HttpSession session = request.getSession();
		String id = (String)session.getAttribute("id");
		KmemberDao md = KmemberDao.getInstance();
		if (id != null) {
			Kmember kme = md.select(id);
			request.setAttribute("Kmember", kme);
		}
		return "login/loginMain";
	}

}

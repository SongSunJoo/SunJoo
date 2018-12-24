package Kbay.service;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;
import Kbay.dao.KmemberDao;
public class DeleteAction implements CommandProcess {
	public String requestPro(HttpServletRequest request, HttpServletResponse response) {
		HttpSession session = request.getSession();
		String id=(String)session.getAttribute("id");
		KmemberDao md = KmemberDao.getInstance();
		int result = md.delete(id);
		// invalidate 세션 지운다는 의미
		if (result > 0)
			session.invalidate();
		// 받아서 보냄
		request.setAttribute("result", result);
		return "login/delete";
	}
	

}

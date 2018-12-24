package Kbay.service;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;
import Kbay.dao.KmemberDao;
import Kbay.model.Kmember;
public class LoginAction implements CommandProcess {
	public String requestPro(HttpServletRequest request, HttpServletResponse response) {
		// jsp파일에서 입력된 id와 pass 받아 옴. 객체명 동일해야 함.
		String id = request.getParameter("id");
		String pass = request.getParameter("pass");
		
		// KmemberDao의 getInstance 객체를 생성함
		// 빨간줄 클릭해서 import
		KmemberDao md = KmemberDao.getInstance();
		// check 메소드를 활용해서 md의 id, pass 객체를 체크함. 그 결과값을 result라 지정 
		Kmember kmember = md.select(id);
		int result = 0;
		if(kmember == null || kmember.getDel().equals("y")) {
			result = -1;
		} else {
			if(kmember.getPass().equals(pass)) {
				HttpSession session = request.getSession();
				session.setAttribute("id", id);
				result = 1;
			} else result = 0;
		}
		request.setAttribute("result", result);
			return "login/login";
		}
	}
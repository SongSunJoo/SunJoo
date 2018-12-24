package Kbay.service;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import Kbay.dao.KmemberDao;
import Kbay.model.Kmember;

public class FindIdAction implements CommandProcess {
	public String requestPro(HttpServletRequest request, HttpServletResponse response) {
		// jsp파일에서 입력된 tel 받아 옴. 객체명 동일해야 함.
		String tel = request.getParameter("tel");
		
		// KmemberDao의 getInstance 객체를 생성. 빨간 줄 뜨면 클릭해서 import
		// dao랑 액션파일 연결
		KmemberDao md = KmemberDao.getInstance();
		Kmember kme	= md.findId(tel);
		
		request.setAttribute("kme", kme);
		
		return "login/findIdResult";
	}
}

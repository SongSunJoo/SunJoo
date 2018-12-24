package Kbay.service;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import Kbay.dao.KmemberDao;
import Kbay.model.Kmember;

public class JoinAction implements CommandProcess {
	public String requestPro(HttpServletRequest request, HttpServletResponse response) {
		// jsp에서 입력된 정보들 받아옴. 
		String id = request.getParameter("id");
		String pass = request.getParameter("pass");
		String name = request.getParameter("name");
		String email = request.getParameter("email");
		String tel = request.getParameter("tel");
		String addr = request.getParameter("addr");
		
		// insert할 때 쓸 변수값. 하나로 묶음
	
		Kmember kme = new Kmember();
		kme.setId(id);
		kme.setPass(pass);
		kme.setName(name);
		kme.setEmail(email);
		kme.setTel(tel);
		kme.setAddr(addr);
		
		int result = 0;
		
		// KmemberDao 핸들링 영역에 객체 생성
		KmemberDao md = KmemberDao.getInstance();
		Kmember km = md.select(kme.getId());
		if (km == null)	result = md.insert(kme);
		else result = -1;
		
		request.setAttribute("result", result);
		return "login/joinResult";
	}

}

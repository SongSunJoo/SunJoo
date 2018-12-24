package Kbay.service;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import Kbay.dao.KmemberDao;

public class IdConfirmAction implements CommandProcess {
	public String requestPro(HttpServletRequest request, HttpServletResponse response) {
		// jsp 파일에서 id 값 받아옴
		String id = request.getParameter("id");
		// 핸들링 영역인 Dao 내 객체 생성 to connect
		KmemberDao md = KmemberDao.getInstance();
		// 핸들링 영역인 Dao 내 id를 기준으로 수식하는 IdConfirm 메소드 만들고 이를 int result에 저장
		int result = md.IdConfirm(id);
		
		request.setAttribute("result", result);
		request.setAttribute("id", id);
		
		return "login/idConfirmResult";
	}
}

package Kbay.service;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import Kbay.dao.KmemberDao;
import Kbay.model.Kmember;
public class UpdateForm implements CommandProcess {
	public String requestPro(HttpServletRequest request, HttpServletResponse response) {
		// update.jsp파일에서 입력된 pass와 name, 주소 받아옴
		// 원래 세션값에서 수정해야 하므로 , getSession
		String id = (String)request.getSession().getAttribute("id");	
		System.out.println("service id="+id);
		
		// KmemberDao 핸들링 영역에 객체 생성
		KmemberDao md = KmemberDao.getInstance();
		// int result로 받는 이유는, 성공하면 1 실패하면 0이라는 숫자로 구분하기 위해서
		Kmember kmember = md.findPass(id);
				
		request.setAttribute("Kmember", kmember);
		return "kmember/updateForm";
		
	}
}

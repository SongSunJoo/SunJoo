package Kbay.service;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import Kbay.dao.KmemberDao;
import Kbay.model.Kmember;
public class MyPageView implements CommandProcess {
	public String requestPro(HttpServletRequest request, HttpServletResponse response) {
		String id=(String)request.getSession().getAttribute("id");

		if (id == null || id.equals(""))
			id = "admin";
		
		System.out.println("myPage id = " + id);
		
		KmemberDao md = KmemberDao.getInstance();
		Kmember kme  = md.select(id);
		
		int result = 0;
		
		if (kme == null || kme.equals("")) {
			System.out.println("kmember null");
		} else
			result = 1;
		
		request.setAttribute("Kmember", kme);
		request.setAttribute("result", result);
		
		return "kmember/myPageView";
	}
}

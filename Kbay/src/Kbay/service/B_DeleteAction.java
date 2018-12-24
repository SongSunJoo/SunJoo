package Kbay.service;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import Kbay.dao.KboardDao;
public class B_DeleteAction implements CommandProcess {
	public String requestPro(HttpServletRequest request, HttpServletResponse response) {
		int num = Integer.parseInt(request.getParameter("b_num"));
		String pageNum = request.getParameter("pageNum");
		
		KboardDao bd = KboardDao.getInstance();
		int result = bd.delete(num);
		
		request.setAttribute("pageNum", pageNum);
		request.setAttribute("result", result);
		
		return "board/B_deleteResult";

	}
}

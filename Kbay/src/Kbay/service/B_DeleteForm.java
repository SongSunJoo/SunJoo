package Kbay.service;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import Kbay.dao.KboardDao;
import Kbay.model.Kboard;
public class B_DeleteForm implements CommandProcess {
	public String requestPro(HttpServletRequest request, HttpServletResponse response) {
		int num = Integer.parseInt(request.getParameter("b_num"));
		String pageNum=request.getParameter("pageNum");
		
		KboardDao bd = KboardDao.getInstance();
		Kboard kboard = bd.select(num);
		
		request.setAttribute("pageNum", pageNum);
		request.setAttribute("kboard", kboard);
		return "board/B_deleteForm";
	}

}

package Kbay.service;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import Kbay.dao.ProductInfoDao;

public class AdminProductDelete implements CommandProcess {

	@Override
	public String requestPro(HttpServletRequest request, HttpServletResponse response) {
		int p_num = Integer.parseInt(request.getParameter("num"));
		String p_class_id1 = request.getParameter("class_id1");
		String p_class_id2 = request.getParameter("class_id2");
		String pageNum = request.getParameter("pageNum");
		ProductInfoDao pd = ProductInfoDao.getInstance();	
		int result = pd.delete(p_num, p_class_id1, p_class_id2);
	
		System.out.println("AdminProductDelete pageNum : " + pageNum);
		System.out.println("AdminProductDelete num : " + p_num);
		System.out.println("AdminProductDelete p_class_id1 : " + p_class_id1);
		System.out.println("AdminProductDelete p_class_id2 : " + p_class_id2);
				
		request.setAttribute("p_class_id1", p_class_id1);
		request.setAttribute("p_class_id2", p_class_id2);
		request.setAttribute("pageNum", pageNum);
		request.setAttribute("result", result);
		
		return "admin/adminProductDelete";
	}

}

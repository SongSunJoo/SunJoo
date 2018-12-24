package Kbay.service;

import java.io.UnsupportedEncodingException;
import java.util.List;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import Kbay.dao.ProductInfoDao;
import Kbay.model.ProductInfo;

public class AdminProductListView implements CommandProcess {
	@Override
	public String requestPro(HttpServletRequest request, HttpServletResponse response) {
		try {
			request.setCharacterEncoding("uft-8");
		} catch (UnsupportedEncodingException e) {	}
		
		String p_class_id1 = request.getParameter("class_id1");
		String p_class_id2 = request.getParameter("class_id2");
		
		int rowPerPage = 10;
		int pagePerBlock = 10;
		String pageNum = request.getParameter("pageNum");
		if (pageNum == null || pageNum.equals("")) {
			pageNum = "1";
		}
		
		int currentPage = Integer.parseInt(pageNum);
		int startRow = (currentPage - 1) * rowPerPage + 1;
		int endRow = startRow + rowPerPage - 1;
		ProductInfoDao pd = ProductInfoDao.getInstance();
		List<ProductInfo> list = pd.list(p_class_id1, p_class_id2, startRow, endRow);
		int tot = pd.total(p_class_id1, p_class_id2);
		int total = tot - startRow + 1;
		int startPage = currentPage - (currentPage - 1) % 10;
		int endPage = startPage + pagePerBlock - 1;
		int totPage = (int)Math.ceil((double)tot/rowPerPage);
		if (endPage > totPage) {
			endPage = totPage;
		}
		
		System.out.println("AdminProductList tot : " + tot);
		System.out.println("AdminProductList total : " + total);
		
		request.setAttribute("currentPage", currentPage);
		request.setAttribute("list", list);
		request.setAttribute("startPage", startPage);
		request.setAttribute("endPage", endPage);
		request.setAttribute("totPage", totPage);
		request.setAttribute("total", total);
		request.setAttribute("pagePerBlock", pagePerBlock);
		request.setAttribute("p_class_id1", p_class_id1);
		request.setAttribute("p_class_id2", p_class_id2);
		
		return "admin/adminProductListView";
	}

}

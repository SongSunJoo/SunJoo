package Kbay.service;

import java.util.List;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import Kbay.dao.ProductInfoDao;
import Kbay.model.ProductInfo;

public class SearchResult implements CommandProcess {
	@Override
	public String requestPro(HttpServletRequest request, HttpServletResponse response) {
		int rowPerPage = 10;
		int pagePerBlock = 10;
		String pageNum = request.getParameter("pageNum");
		if (pageNum == null || pageNum.equals("")) {
			pageNum = "1";
		}
		
		String word = request.getParameter("word");
		if (word == null || word.equals("")) {
			word = "";
		}
		
		int currentPage = Integer.parseInt(pageNum);
		int startRow = (currentPage - 1) * rowPerPage + 1;
		int endRow = startRow + rowPerPage - 1;
		ProductInfoDao pd = ProductInfoDao.getInstance();
		List<ProductInfo> searchList = pd.search(word, startRow, endRow);
		int searchTot = pd.searchTotal(word);
		int searchTotal = searchTot - startRow + 1;
		int startPage = currentPage - (currentPage - 1) % 10;
		int endPage = startPage + pagePerBlock - 1;
		int totPage = (int)Math.ceil((double)searchTot/rowPerPage);
		if (endPage > totPage) {
			endPage = totPage;
		}
		
		request.setAttribute("searchList", searchList);
		request.setAttribute("searchTotal", searchTotal);
		request.setAttribute("startPage", startPage);
		request.setAttribute("endPage", endPage);
		request.setAttribute("totPage", totPage);
		request.setAttribute("pagePerBlock", pagePerBlock);
		request.setAttribute("currentPage", currentPage);
		
		System.out.println("SearchResult searchList : " + searchList);
		System.out.println("SearchResult searchTotal : " + searchTotal);
		System.out.println("SearchResult startPage : " + startPage);
		System.out.println("SearchResult endPage : " + endPage);
		System.out.println("SearchResult totPage : " + totPage);
		System.out.println("SearchResult pagePerBlock : " + pagePerBlock);
		System.out.println("SearchResult currentPage : " + currentPage);
		
		return "search/searchResult";
	}

}

package Kbay.service;

import java.util.List;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import Kbay.dao.ProductDao;
import Kbay.model.Product;

public class NewItemAction implements CommandProcess {

	@Override
	public String requestPro(HttpServletRequest request, HttpServletResponse response) {
		int startNum = 1;
		int endNum = 5;
		
		ProductDao pd = ProductDao.getInstance();
		List<Product> newItem = pd.newItem(startNum, endNum);
		
		request.setAttribute("startNum", startNum);
		request.setAttribute("endNum", endNum);
		request.setAttribute("newItem", newItem);
		
		System.out.println("New Item startNum : " + startNum);
		System.out.println("New Item endNum : " + endNum);
		System.out.println("New Item newItem : " + newItem);
		
		return "main/newItem";
	}

}

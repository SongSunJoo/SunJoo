package Kbay.dao;

import java.io.Reader;
import java.util.HashMap;
import java.util.List;

import org.apache.ibatis.io.Resources;
import org.apache.ibatis.session.SqlSession;
import org.apache.ibatis.session.SqlSessionFactory;
import org.apache.ibatis.session.SqlSessionFactoryBuilder;

import Kbay.model.Kmember;
import Kbay.model.Product;
import Kbay.model.ProductInfo;

public class ProductInfoDao {
	// singleton
	private static ProductInfoDao instance = new ProductInfoDao();
	private ProductInfoDao() { }
	public static ProductInfoDao getInstance() {
		return instance;
	}
	
	// Session 생성, DB 연결
	private static SqlSession session;
	static {
		try {
			Reader reader = Resources.getResourceAsReader("configuration.xml");	
			SqlSessionFactory ssf = new SqlSessionFactoryBuilder().build(reader);
			session = ssf.openSession(true);
			reader.close();
		} catch (Exception e) {
			System.out.println("ProductInfo Session 생성 : " + e.getMessage());
		}
	}
	
	public List<ProductInfo> list1(String id, int startRow, int endRow) {
		//System.out.println("dao id = "+ id);
		HashMap<String, Object> hm = new HashMap<>(); // 여러값이 들어갈 때 키값을 Object를 준다
		hm.put("id", id);
		hm.put("startRow", startRow);
		hm.put("endRow", endRow);
		return session.selectList("productInfons.list1", hm);
	}
	
	public List<ProductInfo> list2(String id, int startRow, int endRow) {
		
		//System.out.println("dao id = "+ id);
		HashMap<String, Object> hm = new HashMap<>(); // 여러값이 들어갈 때 키값을 Object를 준다
		hm.put("id", id);
		hm.put("startRow", startRow);
		hm.put("endRow", endRow);
		return session.selectList("productInfons.list2", hm);
	}
	
	public List<ProductInfo> list3(String id, int startRow, int endRow) {
		HashMap<String, Object> hm = new HashMap<>(); // 여러값이 들어갈 때 키값을 Object를 준다
		hm.put("id", id);
		hm.put("startRow", startRow);
		hm.put("endRow", endRow);
		return session.selectList("productInfons.list3", hm);
	}
	
	// 상품리스트 - 공통
	public List<ProductInfo> list(String p_class_id1, String p_class_id2, int startRow, int endRow) {
		HashMap<String, Object> hm = new HashMap<>(); // 여러값이 들어갈 때 키값을 Object를 준다
		hm.put("p_class_id1", p_class_id1);
		hm.put("p_class_id2", p_class_id2);
		hm.put("startRow", startRow);
		hm.put("endRow", endRow);
		System.out.println("Dao p_class_id1 : " + p_class_id1);
		System.out.println("Dao p_class_id2 : " + p_class_id2);
		System.out.println("Dao startRow : " + startRow);
		System.out.println("Dao endRow : " + endRow);
		System.out.println("Dao hm : " + hm);
		return session.selectList("productInfons.list", hm);
	}
	
	// 페이징 총 Total
	public int total(String p_class_id1, String p_class_id2) {
		HashMap<String, Object> hm = new HashMap<>(); // 여러값이 들어갈 때 키값을 Object를 준다
		hm.put("p_class_id1", p_class_id1);
		hm.put("p_class_id2", p_class_id2);
		System.out.println("total p_class_id1 : " + p_class_id1);
		System.out.println("total p_class_id2 : " + p_class_id2);
		return (int) session.selectOne("productInfons.total", hm);
	}
		
	// 등록상품목록 - 페이징 총 Total
	public int listTotal1(String id) {
		
		return (int) session.selectOne("productInfons.listTotal1", id);
	}
	
	// 경매참여목록 - 페이징 총 Total
	public int listTotal2(String id) {
		
		return (int) session.selectOne("productInfons.listTotal2", id);
	}
	
	// 상품구매목록 - 페이징 총 Total
	public int listTotal3(String id) {
		
		return (int) session.selectOne("productInfons.listTotal3", id);
	}
		
	public int insert(ProductInfo product) {
		int result = 0;
		System.out.println("id : " + product.getA_id());
		System.out.println("p_auct_id : " + product.getP_auct_id());
		System.out.println("p_class_id1 : " + product.getP_class_id1());
		System.out.println("p_class_id2 : " + product.getP_class_id2());
		/*id varchar2(100) not null references kmember(id),
		p_auct_id number(10) not null references auction_result(p_auct_id),
		p_class_id1 varchar2(100) not null references productClass1(p_class_id1),
		p_class_id2 varchar2(100) not null references productClass2(p_class_id2)*/
		result = session.insert("productInfons.insert", product);
		
		return result;
	}
	public int selectMax() {
		int p_num = 0;
		
		p_num = (int) session.selectOne("productInfons.selectMax");
		
		return p_num;
	}
	public String getClass1Name(String p_class_id1) {
		String className = "";
		
		className = (String) session.selectOne("productInfons.getClass1Name", p_class_id1);
		
		return className;
	}
	public String getClass2Name(String p_class_id2) {
		String className = "";
		
		className = (String) session.selectOne("productInfons.getClass2Name", p_class_id2);
		
		return className;
	}
	public String getAuctDesc(int p_auct_id) {
		String auctDesc = "";
		
		auctDesc = (String) session.selectOne("productInfons.getAuctDesc", p_auct_id);
		
		return auctDesc;
	}
	public ProductInfo getDetail(int p_num) {
		
		return (ProductInfo) session.selectOne("productInfons.getDetail", p_num);
	}
	public int getAuctPrice(int p_num) {
		int max_price = 0;
		
		max_price = (int) session.selectOne("productInfons.getAuctPrice", p_num);
		
		return max_price; 
	}
	public List<ProductInfo> getAuctHistory(int p_num) {
		return session.selectList("productInfons.getAuctHistory", p_num);
	}
	public int insertAuct(ProductInfo pInfo) {
		int result = 0;
		
		result = session.insert("productInfons.insertAuct", pInfo);
		
		return result;
	}
	public int updateImgName(int p_num, String p_image) {
		int result = 0;
		HashMap<String, Object> hm = new HashMap<>();
		hm.put("p_num", p_num);
		hm.put("p_image", p_image);
		
		result = session.update("productns.updateImgName", hm);
		return result;
	}
	
	public int delete(int p_num, String p_class_id1, String p_class_id2) {
		HashMap<String, Object> hm = new HashMap<>(); // 여러값이 들어갈 때 키값을 Object를 준다
		hm.put("p_class_id1", p_class_id1);
		hm.put("p_class_id2", p_class_id2);
		hm.put("p_num", p_num);
		return session.delete("productInfons.delete", hm);
	}
	public List<ProductInfo> auctCheck() {
		return session.selectList("productInfons.auctlist");
	}
	public String getA_ID(int p_num) {
		String a_id = "";
		a_id = (String) session.selectOne("productInfons.getA_ID", p_num);
		return a_id;
	}
	public int setAuct1(int p_num, String a_id) {
		int result = 0;
		HashMap<String, Object> hm = new HashMap<>();
		hm.put("p_num", p_num);
		hm.put("a_id", a_id);
		
		result = session.update("productInfons.setAuct1", hm);
		return result;
	}
	public int setAuct4(int p_num) {
		int result = 0;
		result = session.update("productInfons.setAuct4", p_num);
		return result;
	}
	public int auctCheckCnt() {
		return (int) session.selectOne("productInfons.auctCheckCnt");
	}
	// HotBidding 메인
	public List<ProductInfo> hotBidding(int startNum, int endNum) {
		HashMap<String, Object> hm = new HashMap<>();
		hm.put("startNum", startNum);
		hm.put("endNum", endNum);
		return session.selectList("productInfons.bidding", hm);
	}
	
	// 검색 기능
	public List<ProductInfo> search(String word, int startRow, int endRow) {
		HashMap<String, Object> hm = new HashMap<>();
		hm.put("word", word);
		hm.put("startRow", startRow);
		hm.put("endRow", endRow);
		return session.selectList("productInfons.searchList", hm);
	}
	
	// 검색 총 개수
	public int searchTotal(String word) {
		return (int) session.selectOne("productInfons.searchTotal", word);
	}
}

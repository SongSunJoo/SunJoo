package Kbay.dao;

import java.io.Reader;

import org.apache.ibatis.io.Resources;
import org.apache.ibatis.session.SqlSession;
import org.apache.ibatis.session.SqlSessionFactory;
import org.apache.ibatis.session.SqlSessionFactoryBuilder;

import Kbay.model.Auction;
import Kbay.model.Product;

public class AuctionDao {
	//singleton
	private static AuctionDao instance = new AuctionDao();
	private AuctionDao() { }
	public static AuctionDao getInstance() {
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
			System.out.println("Auction Session 생성 : " + e.getMessage());
		}
	}
	public int insert(Auction auction) {
		int result = 0;
		
		result = session.insert("auctionns.insert", auction);
		
		return result;
	}
	
}

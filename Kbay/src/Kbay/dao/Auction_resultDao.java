package Kbay.dao;

import java.io.Reader;

import org.apache.ibatis.io.Resources;
import org.apache.ibatis.session.SqlSession;
import org.apache.ibatis.session.SqlSessionFactory;
import org.apache.ibatis.session.SqlSessionFactoryBuilder;

public class Auction_resultDao {
	//singleton
	private static Auction_resultDao instance = new Auction_resultDao();
	private Auction_resultDao() { }
	public static Auction_resultDao getInstance() {
		return instance;
	}
	
	//Session 생성, DB 연결
	private static SqlSession session;
	static {
		try {
			Reader reader = Resources.getResourceAsReader("configuration.xml");
			SqlSessionFactory ssf = new SqlSessionFactoryBuilder().build(reader);
			session = ssf.openSession(true);
			reader.close();
		} catch (Exception e) {
			System.out.println("Auction Result Session 생성 : " + e.getMessage());
		}
	}
}

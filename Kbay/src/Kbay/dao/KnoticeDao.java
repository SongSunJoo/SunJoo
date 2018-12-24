package Kbay.dao;

import java.io.Reader;
import java.util.HashMap;
import java.util.List;

import org.apache.ibatis.io.Resources;
import org.apache.ibatis.session.SqlSession;
import org.apache.ibatis.session.SqlSessionFactory;
import org.apache.ibatis.session.SqlSessionFactoryBuilder;

import Kbay.model.Knotice;

public class KnoticeDao {
	
	/*singleton 정의*/
	private static KnoticeDao instance = new KnoticeDao();
	private KnoticeDao() {}
	public static KnoticeDao getInstance() {
		return instance;
	}
	
	private static SqlSession session;
	static {
		try {
			Reader reader = Resources.getResourceAsReader("configuration.xml");
			SqlSessionFactory ssf = new SqlSessionFactoryBuilder().build(reader);
			
			session = ssf.openSession(true);	// true: auto commit, 반드시 기술해야함
			reader.close();
		} catch (Exception e) {
			System.out.println("session 생성에러: "+e.getMessage());
		}
	}
	
	// 공지사항 추가
	public int insert(Knotice knotice) {
		return session.insert("knoticens.insert", knotice);
	}
	
	// 공지사항 목록
	public List<Knotice> list(int startRow, int endRow) {
		System.out.println("knotice dao start");
		System.out.println("startRow = " + startRow + ", endRow = " + endRow);
		
		HashMap<String, Object> hm = new HashMap<>(); // HashMap<키(String),오브젝트>
		hm.put("startRow", startRow);
		hm.put("endRow", endRow);
		return session.selectList("knoticens.list", hm);
	}
	
	// 공지사항 총 개수
	public int total() {
		return (int) session.selectOne("knoticens.total");
	}
	
	// 공지사항 내용 보기
	public Knotice select(int n_num) {
		return (Knotice) session.selectOne("knoticens.select", n_num);
	}
	
	// 공지사항 수정
	public int update(Knotice knotice) {
		return session.update("knoticens.update", knotice);
	}
	
	// 공지사항 삭제
	public int delete(int n_num) {
		return session.delete("knoticens.delete", n_num);
	}	
}

package Kbay.dao;
import java.io.Reader;
import java.sql.*;
import java.util.HashMap;
import java.util.List;
import org.apache.ibatis.io.Resources;
import org.apache.ibatis.session.SqlSession;
import org.apache.ibatis.session.SqlSessionFactory;
import org.apache.ibatis.session.SqlSessionFactoryBuilder;
import Kbay.model.Kboard;
import Kbay.model.Kmember;
public class KboardDao {   
	private static final Object board = null;
	// singleton
	private static KboardDao instance = new KboardDao();
	private KboardDao() {}
	public static KboardDao getInstance() {
		return instance;
	}
	private static SqlSession session;
		static {
			try {
			// 파일을 읽어 옴
			Reader reader = Resources.getResourceAsReader("/configuration.xml");
			// 객체 생성 - 저장할 공간을 만들어 줌 
			// 위의 reader를 build하겠다
			SqlSessionFactory ssf = new SqlSessionFactoryBuilder().build(reader);
			session = ssf.openSession(true);
			// true를 쓰면 오토 커밋(자동 저장)
			reader.close(); 
			//읽은거 닫아줌
			} catch (Exception e) { // exception을 잡기 위해서 상위 양식 {
				System.out.println("Session 생성시 에러:"+e.getMessage());
			}
		}
		
		/*---------------------List---------------------------*/
		public List<Kboard> list(int startRow, int endRow) {
			// 파라미터로는 단수개의 정보만 전달 가능
			// List는 특정틀의 동일한 유형을 가진 정보를 전달 가능 But Array로 집합, 정렬, 배열해야 함
			// Hash은 다수의 정보 전달 가능. key값으로 해당 정보 더미들 구분
			//    key값은 문자열로 받고,  정보값은 문자와 숫자 모두로 받을 수 있게 Object로 지정
			HashMap<String, Object> hm = new HashMap<>();
				hm.put("startRow", startRow);
				hm.put("endRow", endRow);
			return session.selectList("kboardns.list", hm);
		}
		
		// 게시글 총 개수
		public int total() {
			return (int) session.selectOne("kboardns.total");
		}
		
		// 게시글 내용 추가
		public int insert(Kboard kboard) {
			int number = (int) session.selectOne("kboardns.numMax");
			
			System.out.println("numMax = "+number);
			
			if (kboard.getB_num()!=0) {
				session.update("kboardns.boardUpdate", kboard);
				kboard.setB_re_step(kboard.getB_re_step()+1);
				kboard.setB_re_level(kboard.getB_re_level()+1);
			} else kboard.setB_ref(number);
			
			kboard.setB_num(number);
			return session.insert("kboardns.insert", kboard);
		}
		
		// 게시글 선택 보기
		public Kboard select(int num) {
			return (Kboard) session.selectOne("kboardns.select", num);
		}
		
		public void readcountUpdate(int num) {		
			session.update("kboardns.readcountUpdate", num);
		}
		
		// 게시글 수정
		public int update(Kboard kboard) {
/*			System.out.println("b_contents:"+kboard.getB_content());
			System.out.println("b_subject:"+kboard.getB_subject());
			System.out.println("b_num:"+kboard.getB_num());*/
			return session.update("kboardns.update", kboard);
		}
		
		public int delete(int num) {
			return session.delete("kboardns.delete", num);
		}
}
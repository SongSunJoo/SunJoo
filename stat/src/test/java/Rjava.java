

import org.rosuda.JRI.REXP;
import org.rosuda.JRI.Rengine;

public class Rjava {
	public static void main(String[] args) {
		String[] Rargs = {"--vanilla"};
		Rengine re = new Rengine(Rargs, false, null);
		System.out.println("Create R Engine...");
		
		if(!re.waitForR()) {
			System.out.println("Loading R engine was failed");
			return;
		}
		
		REXP a = re.eval("a <- 30", true);
		System.out.println(a.asDouble());
//		re.eval("plot(iris$Sepal.Width, iris$Sepal.Length, cex = 0.5, pch = 20, xlab = \"너비\", ylab = \"길이\", main = \"붓꽃\")");
	}
}

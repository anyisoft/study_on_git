import java.util.Arrays;

public class UseGeneric_1 {
    public static void main(String[] args) {
        String[] ss = new String[] { "Orange", "Apple", "Pear" };
		Arrays.sort(ss);
		
		System.out.println( Arrays.toString(ss) );
    }
}
import java.util.Arrays;

public class UseGeneric_2 {
    public static void main(String[] args) {
        Person[] ps = new Person[] {
			new Person("Bob", 61),
			new Person("Alice", 88),
			new Person("Lily", 75),
		};
		
		Arrays.sort(ps);
		System.out.println( Arrays.toString(ps) );
    }
}

class Person {
	String name;
	int    score;
	
	Person(String name, int score) {
		this.name = name;
		this.score = score;
	}
	
	public String toString() {
		return this.name + "," + this.score;
	}
}
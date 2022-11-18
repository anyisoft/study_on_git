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

class Person implements Comparable<Person> {
	String name;
	int    score;
	
	Person(String name, int score) {
		this.name = name;
		this.score = score;
	}
	
	public String toString() {
		return this.name + "," + this.score;
	}
	
	public int compareToV1( Person other ) {
		return this.name.compareTo(other.name);
	}
	
	public int compareTo( Person other ) {
		return this.score - other.score;
	}
}
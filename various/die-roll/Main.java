public class Main {

	public static void main(String[] args) {
		Die die = new Die();  // Create the PairOfDice object.
		String code = "";
		for (int i = 0; i < 5; i++) {
			die.roll();
			code += die.getDie();
		}

		System.out.println(code);
	}
}

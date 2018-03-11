public class Die {
     
    private int die;   // Number showing on the die.

    public Die() {
		// Rolls the die, so that it initially shows some random values.
		roll();
	}

	public void roll() {
		// Roll the die by setting it to be a random number between 1 and 6.
		die = (int) (Math.random() * 6) + 1;
	}

	public int getDie() {
		// Return the number showing on the first die.
		return die;
	}
}

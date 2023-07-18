package util;

public class BitOperations {

    /**
     * Returns whether the bit {code whichBit} is 1.
     * @param whichBit must be between 0-31.
     */
    public static boolean isBitSet(int value, int whichBit) {
        if (whichBit >= 0 && whichBit < 32) {
            return ((value >>> whichBit) & 1) != 0;
        } else {
            throw new IllegalArgumentException("whichBit must be between 0-31");
        }
    }

    /**
     * Returns an integer which contains {@code howMany} 1's.
     * {@code getAllOnes(0)} returns 0, {@code getAllOnes(1)} returns 0B1, {@code getAllOnes(2)} returns 0B11, etc.
     * @param howMany must be between 0-31.
     */
    public static int getAllOnes(int howMany) {
        if (howMany >= 0 && howMany < 32) {
            return (1 << howMany) - 1;
        } else {
            throw new IllegalArgumentException("howMany must be between 0-31");
        }
    }

    /**
     * Removes the most significant bits from {@code value} to only keep {@code howMany} least significant ones.
     * {@code keepOnlyLeastSignificantBits(0B11101, 3)} returns 0B101.
     * @param howMany must be between 0-31
     */
    public static int keepOnlyLeastSignificantBits(int value, int howMany) {
        return value & getAllOnes(howMany);
    }
}

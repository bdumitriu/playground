package util;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;
import static util.BitOperations.*;

public class BitOperationsTest {

    @Test
    public void isBitSetTest() {
        assertFalse(isBitSet(0, 4));
        assertTrue(isBitSet(0B101010101, 0));
        assertFalse(isBitSet(0B101010101, 1));
        assertTrue(isBitSet(0B101010101, 2));
        assertFalse(isBitSet(0B101010101, 3));
        assertTrue(isBitSet(0B101010101, 4));
        assertFalse(isBitSet(0B101010101, 5));
        assertTrue(isBitSet(0B101010101, 6));
        assertFalse(isBitSet(0B101010101, 7));
        assertTrue(isBitSet(0B101010101, 8));
        assertTrue(isBitSet(0B101010101, 8));
        assertThrows(IllegalArgumentException.class, () -> isBitSet(0, 32));
        assertThrows(IllegalArgumentException.class, () -> isBitSet(0, -1));
    }

    @Test
    public void getAllOnesTest() {
        assertEquals(0B0, getAllOnes(0));
        assertEquals(0B1, getAllOnes(1));
        assertEquals(0B11, getAllOnes(2));
        assertEquals(0B111, getAllOnes(3));
        assertEquals(0B1111111111111111111111111111111, getAllOnes(31));
        assertThrows(IllegalArgumentException.class, () -> getAllOnes(32));
        assertThrows(IllegalArgumentException.class, () -> getAllOnes(-1));
    }

    @Test
    public void keepOnlyLeastSignificantBitsTest() {
        assertEquals(0B0, keepOnlyLeastSignificantBits(0B11101, 0));
        assertEquals(0B101, keepOnlyLeastSignificantBits(0B11101, 3));
        assertEquals(0B1111, keepOnlyLeastSignificantBits(0B1111001111, 6));
        assertThrows(IllegalArgumentException.class, () -> keepOnlyLeastSignificantBits(0, 32));
        assertThrows(IllegalArgumentException.class, () -> keepOnlyLeastSignificantBits(0, -1));
    }
}

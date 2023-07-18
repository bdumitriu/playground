import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

public class TetrisBoardTest {

    @Test
    void testFillLineWithNothing() {
        final TetrisBoard tetrisBoard = new TetrisBoard(10, 10);
        tetrisBoard.fillLine(0, 0);
        assertEquals(0, tetrisBoard.getLineBits(0));
    }

    @Test
    void testFillEachSinglePositionInLine() {
        final TetrisBoard tetrisBoard = new TetrisBoard(10, 10);
        for (int i = 0; i < tetrisBoard.getWidth(); i++) {
            tetrisBoard.fillLine(0, 1 << i);
            assertEquals(1 << i, tetrisBoard.getLineBits(0));
        }
    }

    @Test
    void testFillLineFully() {
        final TetrisBoard tetrisBoard = new TetrisBoard(10, 10);
        tetrisBoard.fillLine(0, 0B1111111111);
        assertEquals(0B1111111111, tetrisBoard.getLineBits(0));
    }

    @Test
    void testFillLineFullyWithMinimalOverflow() {
        final TetrisBoard tetrisBoard = new TetrisBoard(10, 10);
        tetrisBoard.fillLine(0, 0B11111111111);
        assertEquals(0B1111111111, tetrisBoard.getLineBits(0));
    }

    @Test
    void testFillLineFullyWithExtendedOverflow() {
        final TetrisBoard tetrisBoard = new TetrisBoard(10, 10);
        tetrisBoard.fillLine(0, 0B1010111010110111111111111);
        assertEquals(0B1111111111, tetrisBoard.getLineBits(0));
    }

    @Test
    void testFailToFillLineBecauseOfOverflow() {
        final TetrisBoard tetrisBoard = new TetrisBoard(10, 10);
        tetrisBoard.fillLine(0, 0B10000000000);
        assertEquals(0, tetrisBoard.getLineBits(0));
    }

    @Test
    void testGetLineOutOfBounds() {
        final TetrisBoard tetrisBoard = new TetrisBoard(10, 10);
        assertThrows(IllegalArgumentException.class, () -> tetrisBoard.getLineBits(-1));
        assertThrows(IllegalArgumentException.class, () -> tetrisBoard.getLineBits(10));
    }

}

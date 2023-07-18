import org.junit.jupiter.api.Test;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.*;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class NextMoveTest {

    @Test
    void testInvertedLMove1() {
        final TetrisBoard tetrisBoard = new TetrisBoard(10, 10);
        final ComputerPlayer computerPlayer = new ComputerPlayer(tetrisBoard);
        tetrisBoard.fillLine(0, 0B1111001111);
        tetrisBoard.injectBlock(new TetrisBlockInvertedL());
        assertThat(computerPlayer.getTargetPositionAndRotation(), anyOf(
                is(new PositionAndRotation(0, 4, 0)),
                is(new PositionAndRotation(0, 2, 3))));
    }

    @Test
    void testInvertedLMove2() {
        final TetrisBoard tetrisBoard = new TetrisBoard(10, 10);
        final ComputerPlayer computerPlayer = new ComputerPlayer(tetrisBoard);
        tetrisBoard.fillLine(4, 0B0001111000);
        tetrisBoard.fillLine(3, 0B1001111110);
        tetrisBoard.fillLine(2, 0B1001111110);
        tetrisBoard.fillLine(1, 0B1001111110);
        tetrisBoard.fillLine(0, 0B1101111111);
        tetrisBoard.injectBlock(new TetrisBlockInvertedL());
        System.out.println(computerPlayer.getTargetPositionAndRotation());
        assertThat(computerPlayer.getTargetPositionAndRotation(), anyOf(
                is(new PositionAndRotation(4, 7, 0)),
                is(new PositionAndRotation(4, 5, 3)),
                is(new PositionAndRotation(5, 3, 0)),
                is(new PositionAndRotation(5, 4, 0)),
                is(new PositionAndRotation(5, 5, 0)),
                is(new PositionAndRotation(5, 3, 1)),
                is(new PositionAndRotation(5, 4, 1))));
    }

    @Test
    void testLMove1() {
        final TetrisBoard tetrisBoard = new TetrisBoard(10, 10);
        final ComputerPlayer computerPlayer = new ComputerPlayer(tetrisBoard);
        tetrisBoard.fillLine(3, 0B0011000110);
        tetrisBoard.fillLine(2, 0B0011001110);
        tetrisBoard.fillLine(1, 0B1011001110);
        tetrisBoard.fillLine(0, 0B1111011111);
        tetrisBoard.injectBlock(new TetrisBlockL());
        assertThat(computerPlayer.getTargetPositionAndRotation(), anyOf(
                is(new PositionAndRotation(4, 2, 0)),
                is(new PositionAndRotation(3, 6, 1))));
    }

    @Test
    void testTMove1() {
        final TetrisBoard tetrisBoard = new TetrisBoard(10, 10);
        final ComputerPlayer computerPlayer = new ComputerPlayer(tetrisBoard);
        tetrisBoard.fillLine(0, 0B0111100000);
        tetrisBoard.injectBlock(new TetrisBlockT());
        assertThat(computerPlayer.getTargetPositionAndRotation(), anyOf(
                is(new PositionAndRotation(0, 0, 1)),
                is(new PositionAndRotation(0, 5, 0)),
                is(new PositionAndRotation(0, 6, 0)),
                is(new PositionAndRotation(0, 7, 0)),
                is(new PositionAndRotation(0, 4, 3))));
    }

    @Test
    void testTMove2() {
        final TetrisBoard tetrisBoard = new TetrisBoard(10, 10);
        final ComputerPlayer computerPlayer = new ComputerPlayer(tetrisBoard);
        tetrisBoard.fillLine(1, 0B0111100000);
        tetrisBoard.fillLine(0, 0B1111000000);
        tetrisBoard.injectBlock(new TetrisBlockT());
        assertThat(computerPlayer.getTargetPositionAndRotation(), anyOf(
                is(new PositionAndRotation(1, 0, 1)),
                is(new PositionAndRotation(0, 5, 0)),
                is(new PositionAndRotation(0, 6, 0)),
                is(new PositionAndRotation(0, 7, 0))));
    }

    @Test
    void testTMove3() {
        final TetrisBoard tetrisBoard = new TetrisBoard(10, 10);
        final ComputerPlayer computerPlayer = new ComputerPlayer(tetrisBoard);
        final TetrisBlockT block = new TetrisBlockT();
        block.rotate();
        tetrisBoard.injectBlock(block);
        assertEquals(3, computerPlayer.getTargetPositionAndRotation().numberOfRotations());
    }

    @Test
    void testSquareMove1() {
        final TetrisBoard tetrisBoard = new TetrisBoard(10, 10);
        final ComputerPlayer computerPlayer = new ComputerPlayer(tetrisBoard);
        tetrisBoard.fillLine(3, 0B0010000001);
        tetrisBoard.fillLine(2, 0B1010110001);
        tetrisBoard.fillLine(1, 0B1010110111);
        tetrisBoard.fillLine(0, 0B1111111111);
        tetrisBoard.injectBlock(new TetrisBlockSquare());
        assertThat(computerPlayer.getTargetPositionAndRotation(), anyOf(
                is(new PositionAndRotation(3, 4, 0)),
                is(new PositionAndRotation(2, 7, 0))));
    }
}

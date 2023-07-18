public interface TetrisBlock {

    int getLeftOffsetAdjustment();

    int getMaxHeight();

    int getMaxWidth();

    int getHeight();

    int getWidth();

    void rotate();

    boolean fills(int line, int column);

    int getLineBits(int line);

    TetrisBlock getClone();
}

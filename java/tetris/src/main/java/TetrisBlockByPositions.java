import util.BitOperations;

public abstract class TetrisBlockByPositions implements TetrisBlock, Cloneable {

    protected int position = 0;

    protected abstract int getNrPositions();

    protected abstract int[][] getSetOfPositions();

    protected abstract int[] getRotationLeftOffsetAdjustment();

    @Override
    public int getLeftOffsetAdjustment() {
        return getRotationLeftOffsetAdjustment()[position];
    }

    @Override
    public void rotate() {
        if (position < getNrPositions() - 1) {
            position++;
        } else {
            position = 0;
        }
    }

    @Override
    public boolean fills(int line, int column) {
        if (line >= 0 && line < getHeight() && column >= 0 && column < getWidth()) {
            return BitOperations.isBitSet(getSetOfPositions()[position][line], getWidth() - column - 1);
        } else {
            return false;
        }
    }

    @Override
    public String toString() {
        return super.toString();
    }

    @Override
    public int getLineBits(int line) {
        if (line >= 0 && line < getHeight()) {
            return getSetOfPositions()[position][line];
        } else {
            return 0;
        }
    }

    @Override
    public TetrisBlock getClone() {
        try {
            return (TetrisBlock) super.clone();
        } catch (CloneNotSupportedException e) {
            throw new RuntimeException(e);
        }
    }
}

public class TetrisBlockSquare extends TetrisBlockByPositions {

    private static final int[][] positions = new int[][] {
            new int[] {
                    0B11,
                    0B11,
            }
    };

    private static final int[] rotationLeftOffsetAdjustment = new int[] {0};

    @Override
    protected int getNrPositions() {
        return 1;
    }

    @Override
    protected int[][] getSetOfPositions() {
        return positions;
    }

    @Override
    protected int[] getRotationLeftOffsetAdjustment() {
        return rotationLeftOffsetAdjustment;
    }

    @Override
    public int getMaxHeight() {
        return 2;
    }

    @Override
    public int getMaxWidth() {
        return 2;
    }

    @Override
    public int getHeight() {
        return 2;
    }

    @Override
    public int getWidth() {
        return 2;
    }
}

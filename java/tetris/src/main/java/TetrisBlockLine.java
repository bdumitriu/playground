public class TetrisBlockLine extends TetrisBlockByPositions {

    private static final int[][] positions = new int[][] {
            new int[] {
                    0B1,
                    0B1,
                    0B1,
                    0B1
            },
            new int[] {
                    0B1111,
            }
    };

    private static final int[] rotationLeftOffsetAdjustment = new int[] {1, -1};

    @Override
    protected int getNrPositions() {
        return 2;
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
        return 4;
    }

    @Override
    public int getMaxWidth() {
        return 4;
    }

    @Override
    public int getHeight() {
        return switch (position) {
            case 0 -> 4;
            case 1 -> 1;
            default -> throw new RuntimeException("Expected position was 0-1.");
        };
    }

    @Override
    public int getWidth() {
        return switch (position) {
            case 0 -> 1;
            case 1 -> 4;
            default -> throw new RuntimeException("Expected position was 0-1.");
        };
    }
}

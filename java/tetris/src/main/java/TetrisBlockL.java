public class TetrisBlockL extends TetrisBlockByPositions {

    private static final int[][] positions = new int[][] {
            new int[] {
                    0B11,
                    0B10,
                    0B10
            },
            new int[] {
                    0B100,
                    0B111,
            },
            new int[] {
                    0B01,
                    0B01,
                    0B11
            },
            new int[] {
                    0B111,
                    0B001
            }
    };

    private static final int[] rotationLeftOffsetAdjustment = new int[] {1, -1, 1, -1};

    @Override
    protected int getNrPositions() {
        return 4;
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
        return 3;
    }

    @Override
    public int getMaxWidth() {
        return 3;
    }

    @Override
    public int getHeight() {
        return switch (position) {
            case 0, 2 -> 3;
            case 1, 3 -> 2;
            default -> throw new RuntimeException("Expected position was 0-3.");
        };
    }

    @Override
    public int getWidth() {
        return switch (position) {
            case 0, 2 -> 2;
            case 1, 3 -> 3;
            default -> throw new RuntimeException("Expected position was 0-3.");
        };
    }
}

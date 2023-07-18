import java.util.LinkedList;
import java.util.Queue;

public class ComputerPlayer {

    private final TetrisBoard tetrisBoard;

    private Queue<Command> commandQueue;

    public ComputerPlayer(TetrisBoard tetrisBoard) {
        if (tetrisBoard == null) {
            throw new IllegalArgumentException("Expected non null tetrisBoard.");
        }
        this.tetrisBoard = tetrisBoard;
    }

    public PositionAndRotation getTargetPositionAndRotation() {
        final TetrisBlock block = tetrisBoard.getCloneOfCurrentBlock();

        for (int i = 0; i < tetrisBoard.getHeight(); i++) {
            for (int j = 0; j <= tetrisBoard.getWidth(); j++) {
                for (int r = 0; r < 4; r++) {
                    if (tetrisBoard.canBlockBeAt(block, i, j) && tetrisBoard.canBlockGetAt(block, i, j)
                            && !tetrisBoard.doesBlockAtCreateNewEmptySpaces(block, i, j)) {
                        return new PositionAndRotation(i, j, r);
                    } else {
                        block.rotate();
                    }
                }
            }
        }

        return null;
    }

    public Command getCommand() {
        TetrisBlock currentBlock = tetrisBoard.getCloneOfCurrentBlock();

        if (commandQueue == null && currentBlock != null) {
            commandQueue = new LinkedList<>();
            final PositionAndRotation positionAndRotation = getTargetPositionAndRotation();
            if (positionAndRotation != null) {
                int leftOffsetOfCurrentBlock = tetrisBoard.getLeftOffsetOfCurrentBlock();
                for (int k = 0; k < positionAndRotation.numberOfRotations(); k++) {
                    commandQueue.add(Command.ROTATE);
                    currentBlock.rotate();
                    leftOffsetOfCurrentBlock += currentBlock.getLeftOffsetAdjustment();
                }
                for (int k = positionAndRotation.leftOffset(); k < leftOffsetOfCurrentBlock; k++) {
                    commandQueue.add(Command.MOVE_LEFT);
                }
                for (int k = leftOffsetOfCurrentBlock; k < positionAndRotation.leftOffset(); k++) {
                    commandQueue.add(Command.MOVE_RIGHT);
                }
            }
        }
        return commandQueue == null ? null : commandQueue.poll();
    }

    public void resetCommandQueue() {
        commandQueue = null;
    }
}

import java.text.MessageFormat;

import static util.BitOperations.*;

public class TetrisBoard {

    private final int[] board;

    private final int width;

    private TetrisBlock currentBlock;

    private int bottomOffsetOfCurrentBlock;

    private int leftOffsetOfCurrentBlock;

    public TetrisBoard(int width, int height) {
        if (width > 0 && width < 32 && height > 9 && height < 51) {
            this.board = new int[height];
            for (int i = 0; i < height; i++) {
                this.board[i] = 0;
            }
            this.width = width;
        } else {
            throw new IllegalArgumentException("Width must be between 1-31, height between 10-50.");
        }
    }

    public int getWidth() {
        return width;
    }

    public int getHeight() {
        return board.length;
    }

    public boolean isFilled(int line, int column) {
        return isBitSet(board[line], width - column - 1) ||
                (currentBlock != null && currentBlock.fills(line - bottomOffsetOfCurrentBlock, column - leftOffsetOfCurrentBlock));
    }

    /**
     * Fills the line {@code lineNumber} of the board (counting from the bottom) with {@code lineBits}. Only the least
     * significant bits are use, up to the width of the board.
     */
    protected void fillLine(int lineNumber, int lineBits) {
        if (lineNumber >= 0 && lineNumber < getHeight()) {
            board[lineNumber] = lineBits & getAllOnes(width);
        }
    }

    /**
     * Return the line {@code lineNumber} of the board (counting from the bottom).
     */
    protected int getLineBits(int lineNumber) {
        if (lineNumber >= 0 && lineNumber < getHeight()) {
            return board[lineNumber];
        } else {
            throw new IllegalArgumentException(MessageFormat.format(
                    "Expected a line number between 0 and {0}. Received: {1}", getHeight(), lineNumber));
        }
    }

    public boolean needsNewBlock() {
        return currentBlock == null;
    }

    public void injectBlock(TetrisBlock block) {
        currentBlock = block;
        bottomOffsetOfCurrentBlock = getHeight() - block.getMaxHeight();
        leftOffsetOfCurrentBlock = (getWidth() - block.getMaxWidth()) / 2;
    }

    public TetrisBlock getCloneOfCurrentBlock() {
        return currentBlock == null ? null : currentBlock.getClone();
    }

    public int getBottomOffsetOfCurrentBlock() {
        return bottomOffsetOfCurrentBlock;
    }

    public int getLeftOffsetOfCurrentBlock() {
        return leftOffsetOfCurrentBlock;
    }

    public void moveBlockDown() {
        if (currentBlock != null) {
            if (canMoveDown()) {
                bottomOffsetOfCurrentBlock--;
            } else {
                transferBlockToBoard();
                clearFullLines();
                currentBlock = null;
            }
        }
    }

    public void rotateBlock() {
        if (currentBlock != null) {
            if (canRotate()) {
                currentBlock.rotate();
                leftOffsetOfCurrentBlock += currentBlock.getLeftOffsetAdjustment();
            }
        }
    }

    public void moveBlockLeft() {
        if (currentBlock != null) {
            if (canMoveLeft()) {
                leftOffsetOfCurrentBlock--;
            }
        }
    }

    public void moveBlockRight() {
        if (currentBlock != null) {
            if (canMoveRight()) {
                leftOffsetOfCurrentBlock++;
            }
        }
    }

    private boolean canMoveDown() {
        return canBlockBeAt(currentBlock, bottomOffsetOfCurrentBlock - 1, leftOffsetOfCurrentBlock);
    }

    private boolean canRotate() {
        final TetrisBlock rotatedBlock;
        rotatedBlock = currentBlock.getClone();
        rotatedBlock.rotate();
        final int leftOffsetAdjustment = currentBlock.getLeftOffsetAdjustment();
        return canBlockBeAt(rotatedBlock, bottomOffsetOfCurrentBlock,leftOffsetOfCurrentBlock + leftOffsetAdjustment);
    }

    private boolean canMoveLeft() {
        return canBlockBeAt(currentBlock, bottomOffsetOfCurrentBlock, leftOffsetOfCurrentBlock - 1);
    }

    private boolean canMoveRight() {
        return canBlockBeAt(currentBlock, bottomOffsetOfCurrentBlock, leftOffsetOfCurrentBlock + 1);
    }

    public boolean canBlockBeAt(TetrisBlock block, int bottomOffset, int leftOffset) {
        if (bottomOffset >= 0 && leftOffset >= 0 && leftOffset <= getWidth() - block.getWidth()) {
            final int nrColumnsToAddToTheRight = getWidth() - leftOffset - block.getWidth();
            for (int i = 0; i < block.getHeight(); i++) {
                final int boardLineBits = getLineBits(bottomOffset + i);
                final int blockLineBitsWithOffsetApplied = block.getLineBits(i) << nrColumnsToAddToTheRight;
                if ((boardLineBits & blockLineBitsWithOffsetApplied) != 0) {
                    return false;
                }
            }
            return true;
        } else {
            return false;
        }
    }

    public boolean canBlockGetAt(TetrisBlock block, int bottomOffset, int leftOffset) {
        for (int i = getHeight() - block.getHeight(); i >= bottomOffset; i--) {
            if (!canBlockBeAt(block, i, leftOffset)) {
                return false;
            }
        }
        return true;
    }

    public boolean doesBlockAtCreateNewEmptySpaces(TetrisBlock block, int bottomOffset, int leftOffset) {
        final int rightOffset = width - leftOffset - block.getWidth();
        int lastLineBits = (bottomOffset == 0) ? getAllOnes(block.getWidth())
                : getBoardLineBitsFromTo(bottomOffset - 1, leftOffset, rightOffset);
        for (int i = 0; i < block.getHeight(); i++) {
            final int blockLineBits = block.getLineBits(i);
            final int boardLineBitsUnderneathBlock =
                    getBoardLineBitsFromTo(bottomOffset + i, leftOffset, rightOffset);
            // lines are scanned from bottom to top to make sure that on no vertical is there a one above a zero. The
            // check works by comparing the next line up with the bitwise AND-aggregated lower lines (used to track the
            // zeros). The approach is based on the observation that a number that has a one in the same (bit) position
            // where another number has a zero will always be bigger
            if ((blockLineBits | boardLineBitsUnderneathBlock) <= lastLineBits) {
                lastLineBits &= blockLineBits | boardLineBitsUnderneathBlock;
            } else {
                return true;
            }
        }
        return false;
    }

    private int getBoardLineBitsFromTo(int lineNumber, int leftOffset, int rightOffset) {
        return keepOnlyLeastSignificantBits(board[lineNumber], width - leftOffset) >> rightOffset;
    }

    private void transferBlockToBoard() {
        if (currentBlock != null) {
            final int nrColumnsToAddToTheRight = getWidth() - leftOffsetOfCurrentBlock - currentBlock.getWidth();
            for (int i = 0; i < currentBlock.getHeight(); i++) {
                final int blockLineBits = currentBlock.getLineBits(i) << nrColumnsToAddToTheRight;
                board[bottomOffsetOfCurrentBlock + i] |= blockLineBits;
            }
        }
    }

    private void clearFullLines() {
        final int fullLine = getAllOnes(width);
        for (int i = 0; i < getHeight(); i++) {
            while (board[i] == fullLine) {
                shiftLinesDownAsOf(i);
            }
        }
    }

    private void shiftLinesDownAsOf(int lineNumber) {
        System.arraycopy(board, lineNumber + 1, board, lineNumber, getHeight() - 1 - lineNumber);
        board[getHeight() - 1] = 0;
    }
}

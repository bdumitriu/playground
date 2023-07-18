public class Main {

    public static void main(String[] args) {
        final TetrisBoard tetrisBoard = new TetrisBoard(10, 30);
        final ComputerPlayer computerPlayer = new ComputerPlayer(tetrisBoard);
        getCommandThread(tetrisBoard, computerPlayer).start();
        getRedrawThread(tetrisBoard).start();
        getBoardThread(tetrisBoard, computerPlayer).run();
    }

    private static Thread getCommandThread(TetrisBoard tetrisBoard, ComputerPlayer computerPlayer) {
        return new Thread(
                () -> {
                    while (true) {
                        synchronized (tetrisBoard) {
                            final Command command = computerPlayer.getCommand();
                            if (command != null) {
                                switch (command) {
                                    case ROTATE -> tetrisBoard.rotateBlock();
                                    case MOVE_RIGHT -> tetrisBoard.moveBlockRight();
                                    case MOVE_LEFT -> tetrisBoard.moveBlockLeft();
                                }
                            }
                        }
                        try {
                            Thread.sleep((long) (Math.random() * 100));
//                            Thread.sleep((long) (500));
                        } catch (InterruptedException e) {
                            throw new RuntimeException(e);
                        }
                    }
                }
        );
    }

    private static Thread getRedrawThread(TetrisBoard tetrisBoard) {
        return new Thread(
                () -> {
                    while (true) {
                        synchronized (tetrisBoard) {
                            redraw(tetrisBoard);
                        }
                        try {
                            Thread.sleep(100);
                        } catch (InterruptedException e) {
                            throw new RuntimeException(e);
                        }
                    }
                }
        );
    }

    private static Thread getBoardThread(TetrisBoard tetrisBoard, ComputerPlayer computerPlayer) {
        return new Thread(
                () -> {
                    while (true) {
                        synchronized (tetrisBoard) {
                            if (tetrisBoard.needsNewBlock()) {
                                tetrisBoard.injectBlock(getRandomBlock());
                                computerPlayer.resetCommandQueue();
                            } else {
                                tetrisBoard.moveBlockDown();
                            }
                        }
                        try {
                            Thread.sleep(100);
                        } catch (InterruptedException e) {
                            throw new RuntimeException(e);
                        }
                    }
                }
        );
    }

    private static void redraw(TetrisBoard tetrisBoard) {
        System.out.print("\033\143");
        new TetrisBoardDrawer(tetrisBoard).draw();
    }

    private static Command getRandomCommand() {
        final double randomValue = Math.random();
        if (randomValue < 0.25) {
            return null;
        } else if (randomValue < 0.5) {
            return Command.MOVE_LEFT;
        } else if (randomValue < 0.75) {
            return Command.MOVE_RIGHT;
        } else {
            return Command.ROTATE;
        }
    }

    private static TetrisBlock getRandomBlock() {
        final double randomValue = Math.random();
        if (randomValue < 0.2) {
            return new TetrisBlockLine();
        } else if (randomValue < 0.4) {
            return new TetrisBlockSquare();
        } else if (randomValue < 0.6) {
            return new TetrisBlockT();
        } else if (randomValue < 0.8) {
            return new TetrisBlockL();
        } else {
            return new TetrisBlockInvertedL();
        }
    }
}

public record TetrisBoardDrawer(TetrisBoard tetrisBoard) {

    public void draw() {
        final int height = tetrisBoard.getHeight();
        final int width = tetrisBoard.getWidth();
        final StringBuilder stringBuilder = new StringBuilder();
        for (int i = height - 1; i >= 0; i--) {
            stringBuilder.append("|");
            for (int j = 0; j < width; j++) {
                stringBuilder.append(tetrisBoard.isFilled(i, j) ? "██" : "  ");
            }
            stringBuilder.append("|\n");
        }
        stringBuilder.append("+");
        stringBuilder.append("--".repeat(Math.max(0, width)));
        stringBuilder.append("+\n");
        System.out.print(stringBuilder);
    }
}

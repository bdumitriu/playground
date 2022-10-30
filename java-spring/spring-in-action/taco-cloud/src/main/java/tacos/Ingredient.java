package tacos;

public record Ingredient(
        String id,
        String name,
        Type type
) {
    public enum Type {
        WRAP, PROTEIN, VEGGIE, CHEESE, SAUCE
    }
}

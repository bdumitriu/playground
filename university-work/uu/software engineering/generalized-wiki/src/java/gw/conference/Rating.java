package gw.conference;

/** Represents/stores a rating for a paper. */
public class Rating {
    
    private String _name;
    private String _value;
    
    public Rating(String name, String value) {
        this._name = name;
        this._value = value;
    }
    
    public String getName() {
        return this._name;    
    }
    
    public String getValue() {
        return this._value;
    }

    
    public String toString() {
        return this._name + " \t " + this._value + " \t " +" \n";
    }

}

package gw.conference;

import java.util.ArrayList;

public class RatingSet extends ArrayList<Rating> {
    private static final long serialVersionUID = 8504762657304149489L;

	public void addRating(Rating r) {
        this.add(r);
    }
    
    public String toString() {
        String toReturn = "";
        
        for(Rating r : this)
            toReturn += "\t" + r.toString();
        
        return toReturn;
    }
}

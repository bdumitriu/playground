package gw.conference;

/** @author LennartKats */

public enum ConferenceState {

	// All states
	
	BEFORE_PAPER_DEADLINE {
		public boolean authorsCanAddPapers() { return true; }
        public boolean authorsCanEditPaper() { return true; }
	},
	BEFORE_REVIEWER_ASSIGN_DEADLINE {
		public boolean pcMembersCanAssignReviewer() { return true; }
        public boolean pcMembersCanReadPapers() { return true; }
	},
	BEFORE_REVIEW_DEADLINE {
		public boolean reviewersCanAddReviews() { return true; }
        public boolean reviewersCanReadPapers() { return true; }
        public boolean pcMembersCanReadPapers() { return true; }
	},
	BEFORE_DISCUSSION_DEADLINE {
        public boolean pcMembersCanReadReviews() { return true; }
		public boolean pcMembersCanAddComments() { return true; }
        public boolean pcMembersCanReadPapers() { return true; }
	},
	BEFORE_FINAL_DEADLINE {
		public boolean authorsCanEditPaper() { return true; }
        public boolean authorsCanReadReviews() { return true; }
        public boolean pcMembersCanReadReviews() { return true; }       
        public boolean pcMembersCanReadPapers() { return true; }
	},
	FINISHED {
		public boolean everyoneCanReadPapers() { return true; }
        public boolean authorsCanReadReviews() { return true; }
        public boolean pcMembersCanReadReviews() { return true; }
        public boolean pcMembersCanReadPapers() { return true; }
	};
	
	// All accessors and their default values
	
    // Paper properties
    
	public boolean authorsCanAddPapers() { return false; }    
    public boolean authorsCanEditPaper() { return false; }
    public boolean authorsCanReadReviews() { return false; }
    
    public boolean reviewersCanAddReviews() { return false; }
    public boolean reviewersCanReadPapers() { return false; }
	
    public boolean pcMembersCanAddComments() { return false; }
    public boolean pcMembersCanReadReviews() { return false; }
    public boolean pcMembersCanReadPapers() { return false; }
    
    public boolean everyoneCanReadPapers() { return false; }    
    
    // Global conference properties

    public boolean pcMembersCanAssignReviewer() { return false; }
	
	// TODO: More accessors, toString() method for storing property?
}

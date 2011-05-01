[
   True                 -- KW["true"],
   False                -- KW["false"],
   Prop                 -- _1,
   Pred                 -- H hs=0 [_1 KW["("] _2 KW[")"]],
   Pred.2:iter-star-sep -- _1 KW[", "],
   Not                  -- H [KW["not"] _1],
   And                  -- H [_1 KW["/\\"] _2],
   Or                   -- H [_1 KW["\\/"] _2],
   Equiv                -- H [_1 KW["<->"] _2],
   ForAll               -- V is=2 [
                             H [KW["forall"] _1 KW[":"]]
			     H [_2]
			   ],
   Exists               -- V is=2 [
                             H [KW["exists"] _1 KW[":"]]
			     H [_2]
			   ],
   Parenthetical        -- H hs=0 [KW["("] _1 KW[")"]]
]

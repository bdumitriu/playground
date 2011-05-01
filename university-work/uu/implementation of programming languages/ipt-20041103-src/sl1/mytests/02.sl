--
-- This test proves that the parser does not read (1) as a TTuple, but rather as
-- an TInt. The fact that the parser computes a 3 without throwing any error is
-- proof of this.
--

(1) + 2

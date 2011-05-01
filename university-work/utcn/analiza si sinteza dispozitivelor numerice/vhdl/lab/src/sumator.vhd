entity sumator is
	port (
		a: in bit;			-- termenul 1
		b: in bit;			-- termenul 2
		c_in: in bit;		-- transportul
		s: out bit;			-- suma
		c_out: out bit		-- transportul generat
	);
end entity sumator;

architecture sumator_tabel_adevar of sumator is
begin
	do: process (a, b, c_in)
	begin
		if a = '0' and b = '0' then
			s <= c_in;
			c_out <= '0';
		elsif (a = '0' and b = '1') or (a = '1' and b = '0') then
			if c_in = '0' then
				s <= '1';
				c_out <= '0';
			else
				s <= '0';
				c_out <= '1';
			end if;
		else
			s <= c_in;
			c_out <= '1';
		end if;
	end process do;
end sumator_tabel_adevar;

architecture sumator_data_flow of sumator is
begin
	s <= a xor b xor c_in;
	c_out <= (a and b) or (a and c_in) or (b and c_in);
end sumator_data_flow;
library ieee;
use ieee.std_logic_1164.all;

entity mux is
	generic (dim: integer);
	port (
		a, b, c, d, e, f, g, h: in std_logic_vector (dim-1 downto 0);
		s: in std_logic_vector (2 downto 0);
		q: out std_logic_vector (dim-1 downto 0));
end entity mux;
				  
architecture mux of mux is   
begin
	with s select
		q <= a when "000",
			 b when "001",
			 c when "010",
			 d when "011",
			 e when "100",
			 f when "101",
			 g when "110",
			 h when "111",
			 h when others;
end mux;
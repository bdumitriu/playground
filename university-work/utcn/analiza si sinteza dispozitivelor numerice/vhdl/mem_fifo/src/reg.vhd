library ieee;
use ieee.std_logic_1164.all;

entity reg is
	generic (dim: integer);
	port (
		en, clk, r: in std_logic;
		d: in std_logic_vector (dim-1 downto 0);
		q: out std_logic_vector (dim-1 downto 0));
end entity reg;
				  
architecture reg of reg is   
begin
	process(clk, r)
	begin
		if (r = '1') then
			for i in q'range loop
				q(i) <= '0';
			end loop;
		elsif (clk'event and clk = '1') then
			q <= d;
		end if;
	end process;
end reg;
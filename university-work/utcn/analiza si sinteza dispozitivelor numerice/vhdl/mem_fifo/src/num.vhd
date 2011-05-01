library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;

entity num is
	generic (dim: integer);
	port (
		en, clk, clr: in std_logic;
		q: buffer std_logic_vector (dim-1 downto 0));
end entity num;
				  
architecture num of num is   
begin
	process(clk, clr)
	begin
		if (clr = '1') then
			for i in q'range loop
				q(i) <= '0';
			end loop;
		elsif (clk'event and clk = '1') then
			q <= q + 1;
		end if;
	end process;
end num;
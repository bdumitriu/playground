library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_unsigned.all;

entity multe_erori is
	port (
		a: in std_logic_vector (3 downto 0);
		b: out std_logic_vector (0 to 3);
		c: in std_logic_vector (3 downto 0)
	);
end multe_erori;

architecture eronata of multe_erori	is
begin
	err: process (c)
	begin
		if c = x"F" then
			b <= a;
		else
			b <= "0101";
		end if;
	end process err;
end eronata;
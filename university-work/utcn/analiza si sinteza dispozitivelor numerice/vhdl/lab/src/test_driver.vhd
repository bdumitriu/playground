library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_unsigned.all;

entity test is
	port (
		a, b, cin: std_logic_vector (3 downto 0);
		s, cout: std_logic_vector (3 downto 0));
end entity test;

architecture test of test is
signal va, vb, vc: std_logic_vector (8 downto 0);
begin
	p1: process (a, b, cin)
	begin
		va(3 downto 0) <= a;
		va(8 downto 4) <= "00000";
		va <= va * 10;
	end process;
end architecture test;
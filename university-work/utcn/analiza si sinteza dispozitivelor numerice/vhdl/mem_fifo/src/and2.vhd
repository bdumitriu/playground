library ieee;
use ieee.std_logic_1164.all;

entity and2 is
	port (
		a, b: in std_logic;
		z: out std_logic);
end entity and2;
				  
architecture and2 of and2 is   
begin
	z <= a and b;
end and2;
library ieee;
use ieee.std_logic_1164.all;

entity three_state is
	generic (dim: integer);
	port (
		data_in: in std_logic_vector (dim-1 downto 0);
		en: in std_logic;
		data_out: out std_logic_vector (dim-1 downto 0));
end entity three_state;

architecture three_state of three_state is
begin
	data_out <= data_in when en = '1' else
				(others => 'Z');
end architecture three_state;
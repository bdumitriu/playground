--
--  File: Decoder.vhd
--  created by Design Wizard: 05/03/01 15:11:27
--

--{{ Section below this comment is automatically maintained
--   and may be overwritten
--{entity {Decoder} architecture {Decoder}}

library IEEE;
use IEEE.std_logic_1164.all;

entity Decoder is
	port (
		DataIn: in STD_LOGIC_VECTOR (3 downto 0);
		Output: out STD_LOGIC_VECTOR (9 downto 0)
		);
end Decoder;

--}} End of automatically maintained section

architecture Decoder of Decoder is
begin

  with DataIn select
    Output <= "1000000000" when "0000",
              "0100000000" when "0001",
              "0010000000" when "0010",
              "0001000000" when "0011",
              "0000100000" when "0100",
              "0000010000" when "0101",
              "0000001000" when "0110",
              "0000000100" when "0111",
              "0000000010" when "1000",
              "0000000001" when "1001",
              "0000000000" when others;
end Decoder;

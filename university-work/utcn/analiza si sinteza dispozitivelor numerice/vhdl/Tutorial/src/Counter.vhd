--
--  File: c:\work\vhdl\Tutorial\SRC\Counter.VHD
--  created by Design Wizard: 05/03/01 15:07:42
--

--{{ Section below this comment is automatically maintained
--   and may be overwritten
--{entity {Counter} architecture {Counter}}

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_unsigned.all;

entity Counter is
	port (
		CLK: in STD_LOGIC;
		RESET: in STD_LOGIC;
		Q: out STD_LOGIC_VECTOR (3 downto 0)
		);
end Counter;

--}} End of automatically maintained section

architecture Counter of Counter is
begin
	
	process (CLK, RESET)
		variable Qint: STD_LOGIC_VECTOR (3 downto 0);
		begin
		if RESET='1' then  
			Qint := "0000"; 
		else
			if CLK'event and CLK='1' then
				if Qint<9 then
					Qint:=Qint+1;
				else
					Qint:="0000";
				end if;
			end if;
		end if;
		Q <= Qint;
	end process;
end Counter;

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

architecture Counter of Counter is
begin

process (CLK, RESET)
variable Qint: STD_LOGIC_VECTOR (3 downto 0);
begin
   if RESET = '1' then  
      Qint := "0000"; 
   else
      if CLK'event and CLK = '1' then
        if Qint < 9 then
          Qint := Qint+1;
        else
          Qint := "0000";
        end if;
     end if;
   end if;
   Q <= Qint;
end process;

end Counter;

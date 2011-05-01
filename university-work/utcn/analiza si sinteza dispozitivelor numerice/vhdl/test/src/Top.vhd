library IEEE;
use IEEE.std_logic_1164.all;

entity Top is
    port (
        RESET: in STD_LOGIC;
        A: in STD_LOGIC;
        B: in STD_LOGIC;
        Count: out STD_LOGIC_VECTOR (3 downto 0)
    );
end Top;

architecture Top_arch of Top is

component Counter is
    port (
        CLK: in STD_LOGIC;
        RESET: in STD_LOGIC;
        Q: out STD_LOGIC_VECTOR (3 downto 0)
    );
end component;

component control is 
  port (A: in STD_LOGIC;
        B: in STD_LOGIC;
        CLK: in STD_LOGIC;
        RESET: in STD_LOGIC;
        DETECTED: out STD_LOGIC);
end component;

signal Advance : STD_LOGIC;
signal Clock : STD_LOGIC;

begin

  CNT : Counter
    port map (Advance,RESET,Count);

  CTRL : control
    port map (A,B,Clock,RESET,Advance);

    process
    begin
        Clock <= '1';
        wait for 10 ns;
        Clock <= '0';
        wait for 10 ns;
    end process;

  
end Top_arch;
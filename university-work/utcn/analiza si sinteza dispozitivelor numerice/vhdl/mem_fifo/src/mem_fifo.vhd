library ieee;
use ieee.std_logic_1164.all;

entity mem_fifo is
	port (
		rd, wr: in std_logic;
		rdinc, wrinc: in std_logic;
		rdptrclr, wrptrclr: in std_logic;
		rst, clk: in std_logic;
		data_in: in std_logic_vector (8 downto 0);
		data_out: out std_logic_vector (8 downto 0));
end entity mem_fifo;

architecture mem_fifo of mem_fifo is
	component reg is
		generic (dim: integer);
		port (
			en, clk, r: in std_logic;
			d: in std_logic_vector (dim-1 downto 0);
			q: out std_logic_vector (dim-1 downto 0));
	end component reg;
	component num is
		generic (dim: integer);
		port (
			en, clk, clr: in std_logic;
			q: buffer std_logic_vector (dim-1 downto 0));
	end component num;
	component dcd is
		port (
			d: in std_logic_vector (2 downto 0);
			q: out std_logic_vector (7 downto 0));
	end component dcd;
	component mux is
		generic (dim: integer);
		port (
			a, b, c, d, e, f, g, h: in std_logic_vector (dim-1 downto 0);
			s: in std_logic_vector (2 downto 0);
			q: out std_logic_vector (dim-1 downto 0));
	end component mux;
	component and2 is
		port (
			a, b: in std_logic;
			z: out std_logic);
	end component and2;
 
	component three_state is
		generic (dim: integer);
		port (
			data_in: in std_logic_vector (dim-1 downto 0);
			en: in std_logic;
			data_out: out std_logic_vector (dim-1 downto 0));
	end component three_state;
 
	type mytype is array (7 downto 0) of std_logic_vector (8 downto 0);
	signal n1_out, n2_out: std_logic_vector (2 downto 0);
	signal d_out: std_logic_vector (7 downto 0);
	signal and_out: std_logic_vector (7 downto 0);
	signal reg_out: mytype;
	signal mux_out: std_logic_vector (8 downto 0);
begin
	num1: num
		generic map (3)
		port map (wrinc, clk, wrptrclr, n1_out);
	
	num2: num
		generic map (3)
		port map (rdinc, clk, rdptrclr, n2_out);
	
	dcd1: dcd
		port map (n1_out, d_out);
	
	and_gates: for i in 7 downto 0 generate
		and1: and2
			port map (wr, d_out(i), and_out(i));
		reg1: reg
			generic map (9)
			port map (and_out(i), clk, rst, data_in, reg_out(i));
	end generate and_gates;
	
	mux1: mux	
		generic map (9)
--		port map (data_in, data_in, data_in, data_in, data_in, 
--			data_in, data_in, data_in, n2_out, mux_out);
		port map (reg_out(0), reg_out(1), reg_out(2), reg_out(3), reg_out(4), 
			reg_out(5), reg_out(6), reg_out(7), n2_out, mux_out);
	
	thrst: three_state
		generic map (9)
		port map (mux_out, rd, data_out);
end architecture mem_fifo;
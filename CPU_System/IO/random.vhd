library ieee;
use ieee.std_logic_1164.all;

entity random is
	port(	clk 	: in 	std_logic;
			rst 	: in 	std_logic;
			rand	: buffer std_logic_vector(31 downto 0)
		);
end;

architecture LFSR of random is
begin
	-----------------------------------------------------------
	-- The 32-bit Psuedo-Random Number Generator based on LFSR
	-- X^32 + X^31 + X^29 + X + 1
	-----------------------------------------------------------
	Psuedo_random: process(clk, rst)
	begin
		if(rst = '1') then			
			rand <= X"01020034"; --hardware initial seed
		elsif(clk'event AND clk = '1') then
			rand <= rand(30 downto 0) & 
				(rand(31) xor rand(30) xor rand(28) xor rand (0));
		end if;		
	end process;
end LFSR;

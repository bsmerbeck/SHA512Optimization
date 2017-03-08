library IEEE;
use IEEE.std_logic_1164.all;

entity sevenseg is
	port(	ain	: in	std_logic_vector(3 downto 0);
			aout	: out std_logic_vector(6 downto 0)
			);
end sevenseg;

architecture decoder of sevenseg is
begin
  aout <= "1000000" when ain="0000" else --0
          "1111001" when ain="0001" else --1
          "0100100" when ain="0010" else --2
          "0110000" when ain="0011" else --3
          "0011001" when ain="0100" else --4
          "0010010" when ain="0101" else --5
          "0000010" when ain="0110" else --6
          "1111000" when ain="0111" else --7
          "0000000" when ain="1000" else --8
          "0011000" when ain="1001" else --9
          "0001000" when ain="1010" else --A
          "0000011" when ain="1011" else --B
          "1000110" when ain="1100" else --C
          "0100001" when ain="1101" else --D
          "0000110" when ain="1110" else --E
          "0001110" when ain="1111" else --F
          "1111111"; -- default
end architecture decoder;

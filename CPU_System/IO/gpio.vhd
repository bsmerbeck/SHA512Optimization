library ieee;
use ieee.std_logic_1164.all;

entity gpio is
	port(	clk	: in std_logic;
			rst	: in std_logic;
			gpio_in	: in std_logic_vector(31 downto 0);
			gpio_out : out std_logic_vector(31 downto 0);
			wr		: in std_logic;
			KEY	: in std_logic_vector(3 downto 0);
			SW		: in std_logic_vector(7 downto 0);
			HEX0, HEX1, HEX2, HEX3: out std_logic_vector(6 downto 0)
		);
end;

architecture gpio of gpio is
	signal outport	: std_logic_vector(31 downto 0);
	signal disp	: std_logic_vector(15 downto 0);
begin					
	display3:	entity work.sevenseg port map (disp(15 downto 12), hex3);
	display2:	entity work.sevenseg port map (disp(11 downto 8), hex2);
	display1:	entity work.sevenseg port map (disp(7 downto 4), hex1);
	display0:	entity work.sevenseg port map (disp(3 downto 0), hex0);
	
	disp <= outport(15 downto 0) when key(3)='1' else outport(31 downto 16);
						
	process(clk, rst) is 
	begin
		if(rst = '1') then
			outport <= (others => '0');
		elsif(clk'event and clk = '1') then
			if(wr='1') then
				outport <= gpio_in;
			end if;		
			gpio_out <= X"000000" & SW;
      end if;
         
   end process;
end gpio;

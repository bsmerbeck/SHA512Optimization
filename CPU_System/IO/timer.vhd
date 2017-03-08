library ieee;
use ieee.std_logic_1164.all;
USE IEEE.numeric_std.all;

entity timer is
   generic(
      speed: natural := 500
   );
   port(
		clk	:  in    std_logic;
		clock_50	:	in	std_logic;	--Use 50MHz for time keeping standard
		rst	:  in    std_logic;
		tin	:  in    std_logic_vector(31 downto 0);
		tout	:  out   std_logic_vector(31 downto 0);
		wr		:  in    std_logic;
		INT	:  out   std_logic;
		IACK	:  in    std_logic
		);
	end;

architecture repeat of timer is
	signal data_reg :  std_logic_vector(31 downto 0);
	signal done :  std_logic;
begin
	tout(0) <= done;
	
	---------------------------------------------------------
	--Generating internal clock according to speed parameter
	--Must run on the 50MHz clock so the time is correct
	---------------------------------------------------------
	slow_clock_generator: process(clock_50, data_reg(0))
		variable loopcount : integer range 0 to 50000000;
	begin
		if(data_reg(0) = '0') then
			loopcount := 0;
			done <= '0';
		elsif(clock_50'event AND clock_50 = '1') then
			if(loopcount = 49999999) then		--50,000,000 cycles of 50MHz clock = 1 second
				done <= '1';
			else
				loopcount := loopcount + 1;
			end if;
		end if;
	end process;
	------------------------------------------------
	--CPU write 32-bit unsigned to data register
	--This number will not be destroyed for
	--repeating time keeping function
	--Timer stops counting when data_reg=0
	--Write 0 to data_reg to stop timer
	------------------------------------------------
	timer_write: process(rst, clk) is
	begin
		if(rst='1') then
			data_reg <= x"00000000";
		elsif(clk'event and clk='1') then
			if(wr='1') then 
				data_reg <= tin; 
			end if; --CPU write handle
		end if;
	end process timer_write;
   -----------------------------------------------
   -- INT Handshaking signal handling;
   -- When count is DONE, set INT 
   -- When IACK is received, reset INT
   -----------------------------------------------
   INT_handshaking : process(rst, iack, done) is 
   begin
      if(rst='1' or iack = '1') then
         int <= '0';
      elsif(done'event and done = '1') then
         int <= '1';
      end if;
   end process;	  
end repeat;

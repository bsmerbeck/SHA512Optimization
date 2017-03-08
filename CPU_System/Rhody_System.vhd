library ieee;
use ieee.std_logic_1164.all;
library altera_mf;
use altera_mf.altera_mf_components.all;

entity Rhody_System is
	port(	CLOCK_50	: in std_logic;
			KEY		: in std_logic_vector(3 downto 0);
			SW			: in std_logic_vector(9 downto 0);
			LEDR		: out std_logic_vector(3 downto 0);
			HEX0, HEX1, HEX2, HEX3: out std_logic_vector(6 downto 0)
		);
end;

architecture DE1 of Rhody_System is
	alias clk : std_logic is clock_50;
	signal rst, nclk : std_logic;
	signal mem_in, mem_out, mem_adr: std_logic_vector(31 downto 0);
	signal prog_out, data_out, stck_out, lib_out: std_logic_vector(31 downto 0);
	signal gpio_out, tout0, rand : std_logic_vector(31 downto 0);
	signal mem_rd, mem_wr: std_logic;
	signal en_prog, en_data, en_stck, en_lib: std_logic;
	signal en_gpio, en_time0, en_rand: std_logic;

	constant MASK_PROG  : std_logic_vector(20 downto 0) := "000000000000000000000";
	constant MASK_DATA  : std_logic_vector(22 downto 0) := "00000000000000000000100";
	constant MASK_STCK  : std_logic_vector(22 downto 0) := "00000000000011111111100";
	constant MASK_LIB   : std_logic_vector(21 downto 0) := "0000000000001111111111";
	constant MASK_TIME0 : std_logic_vector(31 downto 0) := X"000F0003";
	constant MASK_GPIO  : std_logic_vector(31 downto 0) := X"000F0007";
	constant MASK_RAND  : std_logic_vector(31 downto 0) := X"000F0008";

begin
	rst <= not key(0);	--reverse polarity of pushbutton
	nclk <= not clk;		--CPU uses falling edge

	--Rhody cpu
--	the_cpu: entity work.Rhody_CPU_basic port map(clk => nclk, rst => rst,  
--						mem_adr => mem_adr, mem_in => mem_in, mem_out => mem_out, 
--						mem_wr => mem_wr, mem_rd => mem_rd, key => key(2), LEDR => LEDR);
						
	--Rhody cpu with instruction pipeline
	the_cpu: entity work.Rhody_CPU_pipelinev43 port map(clk => nclk, rst => rst,  
						mem_adr => mem_adr, mem_in => mem_in, mem_out => mem_out, 
						mem_wr => mem_wr, mem_rd => mem_rd, key => key(2), LEDR => LEDR);
						
--	the_cpu: entity work.Rhody_CPU_pipelinev2 port map(clk => nclk, rst => rst,  
--						mem_adr => mem_adr, mem_in => mem_in, mem_out => mem_out, 
--						mem_wr => mem_wr, mem_rd => mem_rd, key => key(2), LEDR => LEDR);

	Memory_Address_Decode: process(clk)	--Enable signals for memory and I/O components
	begin
		if(clk'event AND clk = '1') then
			if(mem_adr(31 downto 11)=MASK_PROG) then en_prog<='1'; else en_prog<='0'; end if;
			if(mem_adr(31 downto 9)=MASK_DATA) then en_data<='1'; else en_data<='0'; end if;
			if(mem_adr(31 downto 9)=MASK_STCK) then en_stck<='1'; else en_stck<='0'; end if;
			if(mem_adr(31 downto 10)=MASK_LIB) then en_lib<='1'; else en_lib<='0'; end if;
			if(mem_adr(31 downto 0)=MASK_GPIO) then en_gpio<='1'; else en_gpio<='0'; end if;
			if(mem_adr(31 downto 0)=MASK_TIME0) then en_time0<='1'; else en_time0<='0'; end if;
			if(mem_adr(31 downto 0)=MASK_RAND) then en_rand<='1'; else en_rand<='0'; end if;
		end if;
	end process;

   -- User Program ROM
	Program_ROM: altsyncram generic map(
		lpm_hint => "ENABLE_RUNTIME_MOD=YES, INSTANCE_NAME=PROG",
		WIDTH_A => 32,
		WIDTHAD_A => 11,
		NUMWORDS_A => 2048,
		operation_mode => "ROM",
      outdata_reg_a => "UNREGISTERED",
		init_file => "time.hex"
		)
   port map(
		clock0 => clk,
		address_a => mem_adr(10 downto 0),
		q_a => prog_out
		);
	MEM_IN <= prog_out when mem_rd='1' and en_prog='1' else (others => 'Z');
	
	-- User Data Memory
	Data_Memory: altsyncram generic map(
		WIDTH_A => 32,
		WIDTHAD_A => 9,
		NUMWORDS_A => 512,
      outdata_reg_a => "UNREGISTERED",
		operation_mode => "SINGLE_PORT"
		)
   port map(
		clock0 => clk,
		address_a => mem_adr(8 downto 0),
		q_a => data_out,
		data_a => MEM_OUT,	
		wren_a => mem_wr and en_data
		);
	MEM_IN <= data_out when mem_rd='1' and en_data='1' else (others => 'Z');

	-- Stack Memory
	Stack_Memory: altsyncram generic map(
		WIDTH_A => 32,
		WIDTHAD_A => 9,			
		NUMWORDS_A => 512,
      outdata_reg_a => "UNREGISTERED",
		operation_mode => "SINGLE_PORT"
		)
   port map(
		clock0 => clk,
		address_a => mem_adr(8 downto 0),
		q_a => stck_out,
		data_a => MEM_OUT,	
		wren_a => mem_wr and en_stck
		);
	MEM_IN <= stck_out when mem_rd='1' and en_stck='1' else (others => 'Z');

   -- Library routines (System Functions) ROM
	SYS_ROM: altsyncram generic map(
		WIDTH_A => 32,
		WIDTHAD_A => 10,
		NUMWORDS_A => 1024,
		operation_mode => "ROM",
      outdata_reg_a => "UNREGISTERED",
		init_file => "Library.hex"
		)
   port map(
		clock0 => clk,
		address_a => mem_adr(9 downto 0),
		q_a => lib_out
		);
	MEM_IN <= lib_out when mem_rd='1' and en_lib='1' else (others => 'Z');     

	-- GPIO (general purpose I/O) ports for switches and 7-segment displays
	switches_and_leds : entity work.gpio port map(
		clk => clk,
		rst => rst,
		gpio_in => MEM_out,
		gpio_out => gpio_out,
		wr => mem_wr and en_gpio, 
		KEY => KEY,
		SW => SW(7 downto 0),
		HEX0 => HEX0, 
		HEX1 => HEX1, 
		HEX2 => HEX2, 
		HEX3 => HEX3
		);
	MEM_IN <= gpio_out when mem_rd='1' and en_gpio='1' else (others => 'Z');

	-- Timer #0
	System_Timer0: entity work.timer
	generic map(
		speed => 50000   )	--speed=50MHz/50000 = 1KHz
	port map(
		clk => clk,
		clock_50 => clock_50,
		rst => rst,
		tin => MEM_OUT,
		tout => tout0,
		wr => mem_wr and en_time0,
		--INT => int(0),	--no interrupt yet
		IACK => '1'	--force ack='1'
		);
		MEM_IN <= tout0 when mem_rd='1' and en_time0='1' else (others => 'Z');   
		
 	--Pseudo-random number generator based on LFSR 
	Pseudo_Random: entity work.random
	port map(
		clk => clk,
		rst => rst,
		rand => rand
		);
	MEM_IN <= rand when mem_rd='1' and en_rand='1' else (others => 'Z');  

end DE1;

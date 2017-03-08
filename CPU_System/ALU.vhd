library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
library lpm;
use lpm.all;

entity alu is
	port (alu_op: in std_logic_vector(2 downto 0);
			operand0, operand1 : in std_logic_vector(31 downto 0);
			n : in std_logic_vector(4 downto 0);
			alu_out : out std_logic_vector(31 downto 0);
			carry, overflow : out std_logic
			);
end;

architecture alu_arch of alu is
	signal add_sub : std_logic;
	signal add_sub_out : std_logic_vector(31 downto 0);
	signal product : std_logic_vector(63 downto 0);
	signal asov, mov: std_logic;
	
	component lpm_add_sub
	generic (
		lpm_direction			: STRING;
		lpm_hint					: STRING;
		lpm_representation	: STRING;
		lpm_type					: STRING;
		lpm_width				: NATURAL
	);
	port (
			dataa		: in std_logic_vector (31 downto 0);
			add_sub	: in std_logic ;
			datab		: in std_logic_vector (31 downto 0);
			overflow	: out std_logic ;
			cout		: out std_logic ;
			result	: out std_logic_vector (31 downto 0)
	);
	end component;
	
begin
	lpm_add_sub_component : lpm_add_sub
		generic map (
			lpm_direction => "unused",
			lpm_hint => "one_input_is_CONSTANT=NO,Cin_used=NO",
			lpm_representation => "SIGNED",
			lpm_type => "LPM_ADD_SUB",
			lpm_width => 32
		)
		port map (
			dataa => operand0,
			add_sub => add_sub,
			datab => operand1,
			overflow => asov,
			cout => carry,			--not(borrow) when SUB
			result => add_sub_out
		);

	add_sub <= '0' when alu_op="010" else '1'; --'0' for sub; '1' for add
	overflow <= mov when alu_op="011" else asov;	--multiplication overflow is different
	mov <= '0' when product(63)=product(31) else '1';
	product <= std_logic_vector(signed(operand0) * signed(operand1)); 
	
	alu_out <= 	add_sub_out when alu_op="000" else
				add_sub_out when alu_op="001" else
				add_sub_out when alu_op="010" else
				product(31 downto 0) when alu_op="011" else
				operand0 and operand1 when alu_op="100" else
				operand0 or  operand1 when alu_op="101" else
				operand0 xor operand1 when alu_op="110" else
				std_logic_vector(rotate_right(unsigned(operand0), to_integer(unsigned(n))));
end alu_arch;
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

entity Rhody_CPU_pipeline is 
  port (	clk		: in	std_logic;
			rst		: in	std_logic;
			MEM_ADR	: out	std_logic_vector(31 downto 0);
			MEM_IN	: in  std_logic_vector(31 downto 0);
			MEM_OUT	: out std_logic_vector(31 downto 0);
	      mem_wr	: out std_logic;
			mem_rd	: out std_logic;
			key 		: in	std_logic;
			LEDR 		: out std_logic_vector(3 downto 0)
		);
end;

architecture Structural of Rhody_CPU_pipeline is
-- state machine: CPU_state
type State_type is (S1, S2);
signal update, stage1, stage2, stage3, stage4: State_type;
-- Register File: 8x32
type reg_file_type is array (0 to 7) of std_logic_vector(31 downto 0);
signal register_file : reg_file_type;
-- Internal registers
signal MDR_in, MDR_out, MAR, PSW: std_logic_vector(31 downto 0);
signal PC, SP: unsigned(31 downto 0);  --unsigned for arithemtic operations
-- Internal control signals
signal operand0, operand1, ALU_out : std_logic_vector(31 downto 0);
signal carry, overflow, zero : std_logic;	
-- Pipeline Istruction registers
signal stall: Boolean;
signal IR2, IR3, IR4: std_logic_vector(31 downto 0);
--Rhody Instruction Format
alias Opcode2: std_logic_vector(5 downto 0) is IR2(31 downto 26);
alias Opcode3: std_logic_vector(5 downto 0) is IR3(31 downto 26);
alias Opcode4: std_logic_vector(5 downto 0) is IR4(31 downto 26);
alias	RX2	: std_logic_vector(2 downto 0) is IR2(25 downto 23); 
alias	RX3	: std_logic_vector(2 downto 0) is IR3(25 downto 23); 
alias	RY2	: std_logic_vector(2 downto 0) is IR2(22 downto 20);
alias 	RZ2		: std_logic_vector(2 downto 0) is IR2(19 downto 17);
alias 	RA2		: std_logic_vector(2 downto 0) is IR2(16 downto 14);
alias	RB2		: std_logic_vector(2 downto 0) is IR2(13 downto 11);
alias	RB3		: std_logic_vector(2 downto 0) is IR2(13 downto 11);
alias 	RC2		: std_logic_vector(2 downto 0) is IR2(10 downto 8);
alias 	RC3		: std_logic_vector(2 downto 0) is IR2(10 downto 8);
alias 	RD2		: std_logic_vector(2 downto 0) is IR2(7 downto 5);
alias 	RE2		: std_logic_vector(2 downto 0) is IR2(4 downto 2);
alias I2		: std_logic_vector(15 downto 0) is IR2(15 downto 0);
alias M2		: std_logic_vector(19 downto 0) is IR2(19 downto 0);
alias M3		: std_logic_vector(19 downto 0) is IR3(19 downto 0);
-- Temporary control signals
	signal tmp1, tmp2, tmp3: unsigned(63 downto 0);
	signal tmpx, tmpy, tmpz, tmpa: std_logic_vector(31 downto 0);
--Condition Codes
alias Z: std_logic is PSW(0);
alias C: std_logic is PSW(1);
alias S: std_logic is PSW(2);
alias V: std_logic is PSW(3);
--Instruction Opcodes
constant NOP  : std_logic_vector(5 downto 0) := "000000";
constant LDM  : std_logic_vector(5 downto 0) := "000100";
constant LDR  : std_logic_vector(5 downto 0) := "000101";
constant LDH  : std_logic_vector(5 downto 0) := "001000";
constant LDL  : std_logic_vector(5 downto 0) := "001001";
constant LDI  : std_logic_vector(5 downto 0) := "001010";
constant MOV  : std_logic_vector(5 downto 0) := "001011";
constant STM  : std_logic_vector(5 downto 0) := "001100";
constant STR  : std_logic_vector(5 downto 0) := "001101";
constant ADD  : std_logic_vector(5 downto 0) := "010000";
constant ADI  : std_logic_vector(5 downto 0) := "010001";
constant SUB  : std_logic_vector(5 downto 0) := "010010";
constant MUL  : std_logic_vector(5 downto 0) := "010011";
constant IAND : std_logic_vector(5 downto 0) := "010100"; --avoid keyword
constant IOR  : std_logic_vector(5 downto 0) := "010101"; --avoid keyword
constant IXOR : std_logic_vector(5 downto 0) := "010110"; --avoid keyword
constant IROR : std_logic_vector(5 downto 0) := "010111"; --avoid keyword
--constant MLOAD0   : std_logic_vector(5 downto 0) := "011001";
--constant MLOAD1   : std_logic_vector(5 downto 0) := "011010";
--constant MLOAD2   : std_logic_vector(5 downto 0) := "011011";
--constant MLOAD3   : std_logic_vector(5 downto 0) := "011100";
constant CMP  : std_logic_vector(5 downto 0) := "101010";
constant CMPI : std_logic_vector(5 downto 0) := "110010";
constant JNZ  : std_logic_vector(5 downto 0) := "100000";
constant JNS  : std_logic_vector(5 downto 0) := "100001";
constant JNC  : std_logic_vector(5 downto 0) := "100011";
constant JNV  : std_logic_vector(5 downto 0) := "100010";
constant JZ   : std_logic_vector(5 downto 0) := "100100";
constant JS   : std_logic_vector(5 downto 0) := "100101";
constant JC   : std_logic_vector(5 downto 0) := "100111";
constant JV   : std_logic_vector(5 downto 0) := "100110";
constant JMP  : std_logic_vector(5 downto 0) := "101000";
constant CALL : std_logic_vector(5 downto 0) := "110000";
constant RET  : std_logic_vector(5 downto 0) := "110100";
constant RETI : std_logic_vector(5 downto 0) := "110101";
constant PUSH : std_logic_vector(5 downto 0) := "111000";
constant POP  : std_logic_vector(5 downto 0) := "111001";
constant SYS  : std_logic_vector(5 downto 0) := "111100";
--constant MAJ  : std_logic_vector(5 downto 0) := "011010";
--constant SUM0 : std_logic_vector(5 downto 0) := "011011";
constant SUM1 : std_logic_vector(5 downto 0) := "111101";
constant SIG0 : std_logic_vector(5 downto 0) := "111110";
constant SIG1 : std_logic_vector(5 downto 0) := "111111";
constant ADD64: std_logic_vector(5 downto 0) := "000001";
constant LDIX : std_logic_vector(5 downto 0) := "000110";
constant STIX : std_logic_vector(5 downto 0) := "000111";
constant T2	  : std_logic_vector(5 downto 0) := "000010";
constant T11  : std_logic_vector(5 downto 0) := "101110";
constant T12  : std_logic_vector(5 downto 0) := "101111";
constant STIX64: std_logic_vector(5 downto 0) := "110001";
----------------------------------------------------------------
--constant k0  := To_StdLogicVector(bit_vector'(X"428a2f98d728ae22");
--constant k1  := To_StdLogicVector(bit_vector'(X"7137449123ef65cd");
--constant k2  := To_StdLogicVector(bit_vector'(X"b5c0fbcfec4d3b2f"));
--constant k3  := To_StdLogicVector(bit_vector'(X"e9b5dba58189dbbc"));
--constant k4  := To_StdLogicVector(bit_vector'(X"3956c25bf348b538"));
--constant k5  := To_StdLogicVector(bit_vector'(X"59f111f1b605d019"));
--constant k6  := To_StdLogicVector(bit_vector'(X"923f82a4af194f9b"));
--constant k7  := To_StdLogicVector(bit_vector'(X"ab1c5ed5da6d8118"));
--constant k8  := To_StdLogicVector(bit_vector'(X"d807aa98a3030242"));
--constant k9  := To_StdLogicVector(bit_vector'(X"12835b0145706fbe"));
--constant k10  := To_StdLogicVector(bit_vector'(X"243185be4ee4b28c"));
--constant k11  := To_StdLogicVector(bit_vector'(X"550c7dc3d5ffb4e2"));
--constant k12  := To_StdLogicVector(bit_vector'(X"72be5d74f27b896f"));
--constant k13  := To_StdLogicVector(bit_vector'(X"80deb1fe3b1696b1"));
--constant k14  := To_StdLogicVector(bit_vector'(X"9bdc06a725c71235"));
--constant k15  := To_StdLogicVector(bit_vector'(X"c19bf174cf692694"));
--constant k16  := To_StdLogicVector(bit_vector'(X"e49b69c19ef14ad2"));
--constant k17  := To_StdLogicVector(bit_vector'(X"efbe4786384f25e3"));
--constant k18  := To_StdLogicVector(bit_vector'(X"0fc19dc68b8cd5b5"));
--constant k19  := To_StdLogicVector(bit_vector'(X"240ca1cc77ac9c65"));
--constant k20  := To_StdLogicVector(bit_vector'(X"2de92c6f592b0275"));
--constant k21  := To_StdLogicVector(bit_vector'(X"4a7484aa6ea6e483"));
--constant k22  := To_StdLogicVector(bit_vector'(X"5cb0a9dcbd41fbd4"));
--constant k23  := To_StdLogicVector(bit_vector'(X"76f988da831153b5"));
--constant k24  := To_StdLogicVector(bit_vector'(X"983e5152ee66dfab"));
--constant k25  := To_StdLogicVector(bit_vector'(X"a831c66d2db43210"));
--constant k26  := To_StdLogicVector(bit_vector'(X"b00327c898fb213f"));
--constant k27  := To_StdLogicVector(bit_vector'(X"bf597fc7beef0ee4"));
--constant k28  := To_StdLogicVector(bit_vector'(X"c6e00bf33da88fc2"));
--constant k29  := To_StdLogicVector(bit_vector'(X"d5a79147930aa725"));
--constant k30  := To_StdLogicVector(bit_vector'(X"06ca6351e003826f"));
--constant k31  := To_StdLogicVector(bit_vector'(X"142929670a0e6e70"));
--constant k32  := To_StdLogicVector(bit_vector'(X"27b70a8546d22ffc"));
--constant k33  := To_StdLogicVector(bit_vector'(X"2e1b21385c26c926"));
--constant k34  := To_StdLogicVector(bit_vector'(X"4d2c6dfc5ac42aed"));
--constant k35  := To_StdLogicVector(bit_vector'(X"53380d139d95b3df"));
--constant k36  := To_StdLogicVector(bit_vector'(X"650a73548baf63de"));
--constant k37  := To_StdLogicVector(bit_vector'(X"766a0abb3c77b2a8"));
--constant k38  := To_StdLogicVector(bit_vector'(X"81c2c92e47edaee6"));
--constant k39  := To_StdLogicVector(bit_vector'(X"92722c851482353b"));
--constant k40  := To_StdLogicVector(bit_vector'(X"a2bfe8a14cf10364"));
--constant k41  := To_StdLogicVector(bit_vector'(X"a81a664bbc423001"));
--constant k42  := To_StdLogicVector(bit_vector'(X"c24b8b70d0f89791"));
--constant k43  := To_StdLogicVector(bit_vector'(X"c76c51a30654be30"));
--constant k44  := To_StdLogicVector(bit_vector'(X"d192e819d6ef5218"));
--constant k45  := To_StdLogicVector(bit_vector'(X"d69906245565a910"));
--constant k46  := To_StdLogicVector(bit_vector'(X"f40e35855771202a"));
--constant k47  := To_StdLogicVector(bit_vector'(X"106aa07032bbd1b8"));
--constant k48  := To_StdLogicVector(bit_vector'(X"19a4c116b8d2d0c8"));
--constant k49  := To_StdLogicVector(bit_vector'(X"1e376c085141ab53"));
--constant k50  := To_StdLogicVector(bit_vector'(X"2748774cdf8eeb99"));
--constant k51  := To_StdLogicVector(bit_vector'(X"34b0bcb5e19b48a8"));
--constant k52  := To_StdLogicVector(bit_vector'(X"391c0cb3c5c95a63"));
--constant k53  := To_StdLogicVector(bit_vector'(X"4ed8aa4ae3418acb"));
--constant k54  := To_StdLogicVector(bit_vector'(X"5b9cca4f7763e373"));
--constant k55  := To_StdLogicVector(bit_vector'(X"682e6ff3d6b2b8a3"));
--constant k56  := To_StdLogicVector(bit_vector'(X"748f82ee5defb2fc"));
--constant k57  := To_StdLogicVector(bit_vector'(X"78a5636f43172f60"));
--constant k58  := To_StdLogicVector(bit_vector'(X"84c87814a1f0ab72"));
--constant k59  := To_StdLogicVector(bit_vector'(X"8cc702081a6439ec"));
--constant k60  := To_StdLogicVector(bit_vector'(X"90befffa23631e28"));
--constant k61  := To_StdLogicVector(bit_vector'(X"a4506cebde82bde9"));
--constant k62  := To_StdLogicVector(bit_vector'(X"bef9a3f7b2c67915"));
--constant k63  := To_StdLogicVector(bit_vector'(X"c67178f2e372532b"));
--constant k64  := To_StdLogicVector(bit_vector'(X"ca273eceea26619c"));
--constant k65  := To_StdLogicVector(bit_vector'(X"d186b8c721c0c207"));
--constant k66  := To_StdLogicVector(bit_vector'(X"eada7dd6cde0eb1e"));
--constant k67  := To_StdLogicVector(bit_vector'(X"f57d4f7fee6ed178"));
--constant k68  := To_StdLogicVector(bit_vector'(X"06f067aa72176fba"));
--constant k69  := To_StdLogicVector(bit_vector'(X"0a637dc5a2c898a6"));
--constant k70  := To_StdLogicVector(bit_vector'(X"113f9804bef90dae"));
--constant k71  := To_StdLogicVector(bit_vector'(X"1b710b35131c471b"));
--constant k72  := To_StdLogicVector(bit_vector'(X"28db77f523047d84"));
--constant k73  := To_StdLogicVector(bit_vector'(X"32caab7b40c72493"));
--constant k74  := To_StdLogicVector(bit_vector'(X"3c9ebe0a15c9bebc"));
--constant k75  := To_StdLogicVector(bit_vector'(X"431d67c49c100d4c"));
--constant k76  := To_StdLogicVector(bit_vector'(X"4cc5d4becb3e42b6"));
--constant k77  := To_StdLogicVector(bit_vector'(X"597f299cfc657e2a"));
--constant k78  := To_StdLogicVector(bit_vector'(X"5fcb6fab3ad6faec"));
--constant k79  := To_StdLogicVector(bit_vector'(X"6c44198c4a475817"));
----------------------------------------------------------------------------
--signal w1  := std_logic_vector(63 downto 0);
--signal w2  := std_logic_vector(63 downto 0);
--signal w3  := std_logic_vector(63 downto 0);
--signal w4  := std_logic_vector(63 downto 0);
--signal w5  := std_logic_vector(63 downto 0);
--signal w6  := std_logic_vector(63 downto 0);
--signal w7  := std_logic_vector(63 downto 0);
--signal w8  := std_logic_vector(63 downto 0);
--signal w9  := std_logic_vector(63 downto 0);
--signal w10  := std_logic_vector(63 dow1to 0);
--signal w11  := std_logic_vector(63 downto 0);
--signal w12  := std_logic_vector(63 downto 0);
--signal w13  := std_logic_vector(63 downto 0);
--signal w14  := std_logic_vector(63 downto 0);
--signal w15  := std_logic_vector(63 downto 0);
--signal w16  := std_logic_vector(63 downto 0);
--signal w17  := std_logic_vector(63 downto 0);
--signal w18  := std_logic_vector(63 downto 0);
--signal w19  := std_logic_vector(63 downto 0);
--signal w20  := std_logic_vector(63 dow2to 0);
--signal w21  := std_logic_vector(63 downto 0);
--signal w22  := std_logic_vector(63 downto 0);
--signal w23  := std_logic_vector(63 downto 0);
--signal w24  := std_logic_vector(63 downto 0);
--signal w25  := std_logic_vector(63 downto 0);
--signal w26  := std_logic_vector(63 downto 0);
--signal w27  := std_logic_vector(63 downto 0);
--signal w28  := std_logic_vector(63 downto 0);
--signal w29  := std_logic_vector(63 downto 0);
--signal w30  := std_logic_vector(63 dow3to 0);
--signal w31  := std_logic_vector(63 downto 0);
--signal w32  := std_logic_vector(63 downto 0);
--signal w33  := std_logic_vector(63 downto 0);
--signal w34  := std_logic_vector(63 downto 0);
--signal w35  := std_logic_vector(63 downto 0);
--signal w36  := std_logic_vector(63 downto 0);
--signal w37  := std_logic_vector(63 downto 0);
--signal w38  := std_logic_vector(63 downto 0);
--signal w39  := std_logic_vector(63 downto 0);
--signal w40  := std_logic_vector(63 dow4to 0);
--signal w41  := std_logic_vector(63 downto 0);
--signal w42  := std_logic_vector(63 downto 0);
--signal w43  := std_logic_vector(63 downto 0);
--signal w44  := std_logic_vector(63 downto 0);
--signal w45  := std_logic_vector(63 downto 0);
--signal w46  := std_logic_vector(63 downto 0);
--signal w47  := std_logic_vector(63 downto 0);
--signal w48  := std_logic_vector(63 downto 0);
--signal w49  := std_logic_vector(63 downto 0);
--signal w50  := std_logic_vector(63 dow5to 0);
--signal w51  := std_logic_vector(63 downto 0);
--signal w52  := std_logic_vector(63 downto 0);
--signal w53  := std_logic_vector(63 downto 0);
--signal w54  := std_logic_vector(63 downto 0);
--signal w55  := std_logic_vector(63 downto 0);
--signal w56  := std_logic_vector(63 downto 0);
--signal w57  := std_logic_vector(63 downto 0);
--signal w58  := std_logic_vector(63 downto 0);
--signal w59  := std_logic_vector(63 downto 0);
--signal w60  := std_logic_vector(63 dow6to 0);
--signal w61  := std_logic_vector(63 downto 0);
--signal w62  := std_logic_vector(63 downto 0);
--signal w63  := std_logic_vector(63 downto 0);
--signal w64  := std_logic_vector(63 downto 0);
--signal w65  := std_logic_vector(63 downto 0);
--signal w66  := std_logic_vector(63 downto 0);
--signal w67  := std_logic_vector(63 downto 0);
--signal w68  := std_logic_vector(63 downto 0);
--signal w69  := std_logic_vector(63 downto 0);
--signal w70  := std_logic_vector(63 dow7to 0);
--signal w71  := std_logic_vector(63 downto 0);
--signal w72  := std_logic_vector(63 downto 0);
--signal w73  := std_logic_vector(63 downto 0);
--signal w74  := std_logic_vector(63 downto 0);
--signal w75  := std_logic_vector(63 downto 0);
--signal w76  := std_logic_vector(63 downto 0);
--signal w77  := std_logic_vector(63 downto 0);
--signal w78  := std_logic_vector(63 downto 0);
--signal w79  := std_logic_vector(63 downto 0);
---------------------------------------------------------------------------
--signal message0 := std_logic_vector(63 downto 0);
--signal message1 := std_logic_vector(63 downto 0);
--signal message2 := std_logic_vector(63 downto 0);
--signal message3 := std_logic_vector(63 downto 0);
--signal message4 := std_logic_vector(63 downto 0);
--signal message5 := std_logic_vector(63 downto 0);
--signal message6 := std_logic_vector(63 downto 0);
--signal message7 := std_logic_vector(63 downto 0);
--signal message8 := std_logic_vector(63 downto 0);



begin
--Display condition code on LEDR for debugging purpose
LEDR(3) <= Z when key='0' else '0';
LEDR(2) <= C when key='0' else '0';
LEDR(1) <= S when key='0' else '0';
LEDR(0) <= V when key='0' else '0';
--CPU bus interface
MEM_OUT <= MDR_out;	--Outgoing data bus
MEM_ADR <= MAR;		--Address bus
--One clock cycle delay in obtaining CPU_state, e.g. S1->S2
mem_rd	<=	'1' when ((Opcode2=LDM or Opcode2=LDR or Opcode2 = LDIX) and stage2=S2) else
				'1' when (stage1=S2 and not stall) else
				'1' when ((Opcode2=POP or Opcode2=RET) and stage2=S2) else
				'1' when (Opcode2=RETI and stage2=S2) else
				'1' when (Opcode3=RETI and stage3=S2) else
				'0';		--Memory read control signal
mem_wr	<=	'1' when ((Opcode3=STM or Opcode3=STR or Opcode3=STIX or Opcode3 = STIX64) and stage3=S1) else
				'1' when ((Opcode3=PUSH or Opcode3=CALL) and stage3=S2) else
				'1' when (Opcode3=SYS and stage3=S2) else	
				'1' when (Opcode4=SYS and stage4=S2) else			
				'0';		--Memory write control signal
stall <= true when(Opcode2=LDM or Opcode2=LDR or Opcode2 = LDIX or Opcode2=STM or Opcode2=STR or Opcode2=STIX or Opcode2 = STIX64) else
			true when(Opcode2=CALL or Opcode2=PUSH or Opcode2=POP or Opcode2=RET
							or Opcode2=SYS or Opcode2=RETI) else
			true when(Opcode3=CALL  or Opcode3=RET	or Opcode3=PUSH
							or Opcode3=SYS or Opcode3=RETI) else
			true when(Opcode4=SYS or Opcode4=RETI) else
			false;
--The state machine that is CPU
CPU_State_Machine: process (clk, rst)
begin
if rst='1' then
	update <= S1;
	stage1 <= S1;
	stage2 <= S1;
	stage3 <= S1;
	stage4 <= S1;
	PC <= x"00000000";	--initialize PC
	SP <= x"000FF7FF";	--initialize SP
	IR2 <= x"00000000";
	IR3 <= x"00000000";
	IR4 <= x"00000000";
elsif clk'event and clk = '1' then

	case update is
		when S1 =>
			update <= S2;
		when S2 =>
			if (stall or
				(Opcode2=JNZ and Z='1') or (Opcode2=JZ and Z='0') or
				(Opcode2=JNS and S='1') or (Opcode2=JS and S='0') or
				(Opcode2=JNV and V='1') or (Opcode2=JV and V='0') or
				(Opcode2=JNC and C='1') or (Opcode2=JC and C='0') ) then
				IR2 <= x"00000000";	--insert NOP
			else
				IR2 <= MEM_in;
			end if;
			IR3 <= IR2;
			IR4 <= IR3;
			update <= S1;
		when others =>
			null;
	end case;

	case stage1 is
		when S1 =>
			if (not stall) then
				if(Opcode2=JMP or Opcode2=JNZ or Opcode2=JZ or Opcode2=JNS or
					Opcode2=JS or Opcode2=JNV or Opcode2=JV or 
					Opcode2=JNC or Opcode2=JC) then
					MAR <= x"000" & M2;
			else
				MAR <= std_logic_vector(PC);
			end if;
		end if;
			stage1 <= S2;
		when S2 =>
			if (not stall) then
				if (Opcode2=JMP or
					 (Opcode2=JNZ and Z='0') or (Opcode2=JZ and Z='1') or
					 (Opcode2=JNS and S='0') or (Opcode2=JS and S='1') or
					 (Opcode2=JNV and V='0') or (Opcode2=JV and V='1') or
					 (Opcode2=JNC and C='0') or (Opcode2=JC and C='1') ) then
				PC <= (x"000" & unsigned(M2))+1;
			elsif ((Opcode2=JNZ and Z='1') or (Opcode2=JZ and Z = '0') or 
			(Opcode2=JNS and S = '1')or (Opcode2=JS and S = '0') or
			(Opcode2=JNV and V = '1') or (Opcode2=JV and V = '0') or
			(Opcode2=JNC and C = '1') or (Opcode2=JC and C = '0')) then
				null;
			else
				PC <= PC + 1;
			end if;
		end if;
			stage1 <= S1;
		when others =>
			null;
	end case;

	case stage2 is
		when S1 =>
			if (Opcode2=LDI) then
				register_file(to_integer(unsigned(RX2)))<=(31 downto 16=>I2(15)) & I2;
			elsif (Opcode2=LDH) then
				register_file(to_integer(unsigned(RX2)))
					<= I2 & register_file(to_integer(unsigned(RX2)))(15 downto 0);
					--(31 downto 16)<= I2;
			elsif (Opcode2=LDL) then
				register_file(to_integer(unsigned(RX2)))
					<= register_file(to_integer(unsigned(RX2)))(31 downto 16) & I2;
					--(15 downto 0)<= I2;
			elsif (Opcode2=MOV) then
				register_file(to_integer(unsigned(RX2)))<=register_file(to_integer(unsigned(RY2)));	
			elsif (Opcode2=ADD or Opcode2=SUB or Opcode2=MUL or Opcode2=CMP or
					 Opcode2=IAND or Opcode2=IOR or Opcode2=IXOR) then
				operand1 <= register_file(to_integer(unsigned(RY2)));
			elsif (Opcode2=IROR) then
				null;
			elsif (Opcode2=ADI or Opcode2=CMPI) then
				operand1 <= (31 downto 16=>I2(15)) & I2;
			elsif (Opcode2=LDM) then
				MAR <= x"000" & M2;
			elsif (Opcode2=LDR) then
				MAR <= register_file(to_integer(unsigned(RY2)));
			elsif (Opcode2=LDIX) then
					MAR <= std_logic_vector(unsigned(
						register_file(to_integer(unsigned(RY2))))
						+ unsigned(M2));
			elsif (Opcode2=STM) then
				MAR <= x"000" & M2;	MDR_out <= register_file(to_integer(unsigned(RX2)));
			elsif (Opcode2=STR) then
				MAR <= register_file(to_integer(unsigned(RX2)));	
				MDR_out <= register_file(to_integer(unsigned(RY2)));
			elsif (Opcode2=STIX) then
					MAR <= std_logic_vector(unsigned(
						register_file(to_integer(unsigned(RX2))))
						+ unsigned(M2));
					MDR_out <=
						register_file(to_integer(unsigned(RY2)));
			elsif (Opcode2=STIX64) then
					MAR <= std_logic_vector(unsigned(
						register_file(to_integer(unsigned(RX2))))
						+ unsigned(M2));
					MDR_out <=
						register_file(to_integer(unsigned(RY2)));
			elsif (Opcode2=JMP or
					 (Opcode2=JNZ and Z='0') or (Opcode2=JZ and Z='1') or
					 (Opcode2=JNS and S='0') or (Opcode2=JS and S='1') or
					 (Opcode2=JNV and V='0') or (Opcode2=JV and V='1') or
					 (Opcode2=JNC and C='0') or (Opcode2=JC and C='1') ) then
				PC <= x"000" & unsigned(M2);
			elsif (Opcode2=CALL or Opcode2=PUSH or Opcode2=SYS) then
				SP <= SP + 1;
			elsif (Opcode2=RET or Opcode2=RETI or Opcode2=POP) then
				MAR <= std_logic_vector(SP);
			--elsif (Opcode2=CH) then
--				register_file(to_integer(unsigned(RX2))) <=
--					(register_file(to_integer(unsigned(RX2))) and register_file(to_integer(unsigned(RZ2))))xor
--					(not register_file(to_integer(unsigned(RX2))) and register_file(to_integer(unsigned(RB2))));
--				register_file(to_integer(unsigned(RY2))) <=
--					(register_file(to_integer(unsigned(RY2))) and register_file(to_integer(unsigned(RA2))))xor
--					(not register_file(to_integer(unsigned(RY2))) and register_file(to_integer(unsigned(RC2))));
				----register_file(to_integer(unsigned(RX2))) <=
					--std_logic_vector(((register_file(to_integer(unsigned(RB2)))& register_file(to_integer(unsigned(RC2)))) xor ((register_file(to_integer(unsigned(RX2))) & register_file(to_integer(unsigned(RY2)))) and ((register_file(to_integer(unsigned(RZ2))) & register_file(to_integer(unsigned(RA2)))) xor (register_file(to_integer(unsigned(RB2))) & register_file(to_integer(unsigned(RC2))))))))(63 downto 32);
				--register_file(to_integer(unsigned(RY2))) <=
					--std_logic_vector(((register_file(to_integer(unsigned(RB2)))& register_file(to_integer(unsigned(RC2)))) xor ((register_file(to_integer(unsigned(RX2))) & register_file(to_integer(unsigned(RY2)))) and ((register_file(to_integer(unsigned(RZ2))) & register_file(to_integer(unsigned(RA2)))) xor (register_file(to_integer(unsigned(RB2))) & register_file(to_integer(unsigned(RC2))))))))(31 downto 0);
--			elsif (Opcode2=MAJ) then
--				
----					(register_file(to_integer(unsigned(RX2))) and register_file(to_integer(unsigned(RZ2))))xor
----					(register_file(to_integer(unsigned(RX2))) and register_file(to_integer(unsigned(RB2))))xor
----					(register_file(to_integer(unsigned(RZ2))) and register_file(to_integer(unsigned(RB2))));
----				register_file(to_integer(unsigned(RY2))) <=
----					(register_file(to_integer(unsigned(RY2))) and register_file(to_integer(unsigned(RA2))))xor
----					(register_file(to_integer(unsigned(RY2))) and register_file(to_integer(unsigned(RC2))))xor
----					(register_file(to_integer(unsigned(RA2))) and register_file(to_integer(unsigned(RC2))));
--				register_file(to_integer(unsigned(RX2))) <=
--				std_logic_vector(((register_file(to_integer(unsigned(RX2))) & register_file(to_integer(unsigned(RY2)))) and (register_file(to_integer(unsigned(RZ2))) & register_file(to_integer(unsigned(RA2)))))xor
--				((register_file(to_integer(unsigned(RX2))) & register_file(to_integer(unsigned(RY2)))) and (register_file(to_integer(unsigned(RB2))) & register_file(to_integer(unsigned(RC2)))))xor
--				((register_file(to_integer(unsigned(RZ2))) & register_file(to_integer(unsigned(RA2)))) and (register_file(to_integer(unsigned(RB2))) & register_file(to_integer(unsigned(RC2))))))(63 downto 32);
--				register_file(to_integer(unsigned(RY2))) <=
--				std_logic_vector(((register_file(to_integer(unsigned(RX2))) & register_file(to_integer(unsigned(RY2)))) and (register_file(to_integer(unsigned(RZ2))) & register_file(to_integer(unsigned(RA2)))))xor
--				((register_file(to_integer(unsigned(RX2))) & register_file(to_integer(unsigned(RY2)))) and (register_file(to_integer(unsigned(RB2))) & register_file(to_integer(unsigned(RC2)))))xor
--				((register_file(to_integer(unsigned(RZ2))) & register_file(to_integer(unsigned(RA2)))) and (register_file(to_integer(unsigned(RB2))) & register_file(to_integer(unsigned(RC2))))))(31 downto 0);
--			elsif (Opcode2=SUM0) then
--				register_file(to_integer(unsigned(RX2))) <= std_logic_vector(std_logic_vector(rotate_right(unsigned(register_file(to_integer(unsigned(RX2)))) & unsigned(register_file(to_integer(unsigned(RY2)))),28)) xor
--						std_logic_vector(rotate_right(unsigned(register_file(to_integer(unsigned(RX2)))) & unsigned(register_file(to_integer(unsigned(RY2)))),34)) xor
--						std_logic_vector(rotate_right(unsigned(register_file(to_integer(unsigned(RX2)))) & unsigned(register_file(to_integer(unsigned(RY2)))),39)))(63 downto 32);
--				register_file(to_integer(unsigned(RY2))) <= std_logic_vector(std_logic_vector(rotate_right(unsigned(register_file(to_integer(unsigned(RX2)))) & unsigned(register_file(to_integer(unsigned(RY2)))),28)) xor
--						std_logic_vector(rotate_right(unsigned(register_file(to_integer(unsigned(RX2)))) & unsigned(register_file(to_integer(unsigned(RY2)))),34)) xor
--						std_logic_vector(rotate_right(unsigned(register_file(to_integer(unsigned(RX2)))) & unsigned(register_file(to_integer(unsigned(RY2)))),39)))(31 downto 0);
			elsif (Opcode2=SUM1) then
				register_file(to_integer(unsigned(RX2))) <= std_logic_vector(std_logic_vector(rotate_right(unsigned(register_file(to_integer(unsigned(RX2)))) & unsigned(register_file(to_integer(unsigned(RY2)))),14)) xor
						std_logic_vector(rotate_right(unsigned(register_file(to_integer(unsigned(RX2)))) & unsigned(register_file(to_integer(unsigned(RY2)))),18)) xor
						std_logic_vector(rotate_right(unsigned(register_file(to_integer(unsigned(RX2)))) & unsigned(register_file(to_integer(unsigned(RY2)))),41)))(63 downto 32);
				register_file(to_integer(unsigned(RY2))) <= std_logic_vector(std_logic_vector(rotate_right(unsigned(register_file(to_integer(unsigned(RX2)))) & unsigned(register_file(to_integer(unsigned(RY2)))),14)) xor
						std_logic_vector(rotate_right(unsigned(register_file(to_integer(unsigned(RX2)))) & unsigned(register_file(to_integer(unsigned(RY2)))),18)) xor
						std_logic_vector(rotate_right(unsigned(register_file(to_integer(unsigned(RX2)))) & unsigned(register_file(to_integer(unsigned(RY2)))),41)))(31 downto 0);
			elsif (Opcode2=SIG0) then
				register_file(to_integer(unsigned(RX2))) <= std_logic_vector(std_logic_vector(rotate_right(unsigned(register_file(to_integer(unsigned(RX2)))) & unsigned(register_file(to_integer(unsigned(RY2)))),1)) xor
						std_logic_vector(rotate_right(unsigned(register_file(to_integer(unsigned(RX2)))) & unsigned(register_file(to_integer(unsigned(RY2)))),8)) xor
						std_logic_vector(shift_right(unsigned(register_file(to_integer(unsigned(RX2)))) & unsigned(register_file(to_integer(unsigned(RY2)))),7)))(63 downto 32);
				register_file(to_integer(unsigned(RY2))) <= std_logic_vector(std_logic_vector(rotate_right(unsigned(register_file(to_integer(unsigned(RX2)))) & unsigned(register_file(to_integer(unsigned(RY2)))),1)) xor
						std_logic_vector(rotate_right(unsigned(register_file(to_integer(unsigned(RX2)))) & unsigned(register_file(to_integer(unsigned(RY2)))),8)) xor
						std_logic_vector(shift_right(unsigned(register_file(to_integer(unsigned(RX2)))) & unsigned(register_file(to_integer(unsigned(RY2)))),7)))(31 downto 0);
			elsif (Opcode2=SIG1) then
				register_file(to_integer(unsigned(RX2))) <= std_logic_vector(std_logic_vector(rotate_right(unsigned(register_file(to_integer(unsigned(RX2)))) & unsigned(register_file(to_integer(unsigned(RY2)))),19)) xor
						std_logic_vector(rotate_right(unsigned(register_file(to_integer(unsigned(RX2)))) & unsigned(register_file(to_integer(unsigned(RY2)))),61)) xor
						std_logic_vector(shift_right(unsigned(register_file(to_integer(unsigned(RX2)))) & unsigned(register_file(to_integer(unsigned(RY2)))),6)))(63 downto 32);
				register_file(to_integer(unsigned(RY2))) <= std_logic_vector(std_logic_vector(rotate_right(unsigned(register_file(to_integer(unsigned(RX2)))) & unsigned(register_file(to_integer(unsigned(RY2)))),19)) xor
						std_logic_vector(rotate_right(unsigned(register_file(to_integer(unsigned(RX2)))) & unsigned(register_file(to_integer(unsigned(RY2)))),61)) xor
						std_logic_vector(shift_right(unsigned(register_file(to_integer(unsigned(RX2)))) & unsigned(register_file(to_integer(unsigned(RY2)))),6)))(31 downto 0);		
			elsif (Opcode2 = ADD64) then
				register_file(to_integer(unsigned(RX2))) <= std_logic_vector((unsigned(register_file(to_integer(unsigned(RX2)))) & unsigned(register_file(to_integer(unsigned(RY2))))) + (unsigned(register_file(to_integer(unsigned(RZ2)))) & unsigned(register_file(to_integer(unsigned(RA2))))))(63 downto 32);
				register_file(to_integer(unsigned(RY2))) <= std_logic_vector((unsigned(register_file(to_integer(unsigned(RX2)))) & unsigned(register_file(to_integer(unsigned(RY2))))) + (unsigned(register_file(to_integer(unsigned(RZ2)))) & unsigned(register_file(to_integer(unsigned(RA2))))))(31 downto 0);
			elsif (Opcode2 = T11) then
				
				register_file(to_integer(unsigned(RX2))) <= 
				std_logic_vector(unsigned(((register_file(to_integer(unsigned(RB2)))& register_file(to_integer(unsigned(RC2)))) xor ((register_file(to_integer(unsigned(RX2))) & register_file(to_integer(unsigned(RY2)))) and ((register_file(to_integer(unsigned(RZ2))) & register_file(to_integer(unsigned(RA2)))) xor (register_file(to_integer(unsigned(RB2))) & register_file(to_integer(unsigned(RC2)))))))) +
				unsigned(std_logic_vector(rotate_right(unsigned(register_file(to_integer(unsigned(RX2)))) & unsigned(register_file(to_integer(unsigned(RY2)))),14)) xor
				std_logic_vector(rotate_right(unsigned(register_file(to_integer(unsigned(RX2)))) & unsigned(register_file(to_integer(unsigned(RY2)))),18)) xor
						std_logic_vector(rotate_right(unsigned(register_file(to_integer(unsigned(RX2)))) & unsigned(register_file(to_integer(unsigned(RY2)))),41))) 
						+ (unsigned(register_file(to_integer(unsigned(RD2)))) & unsigned(register_file(to_integer(unsigned(RE2)))) + 0))(63 downto 32);
				register_file(to_integer(unsigned(RY2))) <= 
				std_logic_vector(unsigned(((register_file(to_integer(unsigned(RB2)))& register_file(to_integer(unsigned(RC2)))) xor ((register_file(to_integer(unsigned(RX2))) & register_file(to_integer(unsigned(RY2)))) and ((register_file(to_integer(unsigned(RZ2))) & register_file(to_integer(unsigned(RA2)))) xor (register_file(to_integer(unsigned(RB2))) & register_file(to_integer(unsigned(RC2)))))))) +
				unsigned(std_logic_vector(rotate_right(unsigned(register_file(to_integer(unsigned(RX2)))) & unsigned(register_file(to_integer(unsigned(RY2)))),14)) xor
				std_logic_vector(rotate_right(unsigned(register_file(to_integer(unsigned(RX2)))) & unsigned(register_file(to_integer(unsigned(RY2)))),18)) xor
						std_logic_vector(rotate_right(unsigned(register_file(to_integer(unsigned(RX2)))) & unsigned(register_file(to_integer(unsigned(RY2)))),41))) 
						+ (unsigned(register_file(to_integer(unsigned(RD2)))) & unsigned(register_file(to_integer(unsigned(RE2)))) + 0))(31 downto 0);
						
				tmpx <= std_logic_vector(register_file(to_integer(unsigned(RX2))));
				tmpy <= std_logic_vector(register_file(to_integer(unsigned(RY2))));
			elsif (Opcode2 = T12) then
				register_file(to_integer(unsigned(RX2))) <= 
					std_logic_vector((unsigned(register_file(to_integer(unsigned(RX2)))) & unsigned(register_file(to_integer(unsigned(RY2))))) + 
					(unsigned(register_file(to_integer(unsigned(RZ2)))) & unsigned(register_file(to_integer(unsigned(RA2))))) +
					(unsigned(register_file(to_integer(unsigned(RB2)))) & unsigned(register_file(to_integer(unsigned(RC2))))))(63 downto 32);
				register_file(to_integer(unsigned(RY2))) <= 
					std_logic_vector((unsigned(register_file(to_integer(unsigned(RX2)))) & unsigned(register_file(to_integer(unsigned(RY2))))) + 
					(unsigned(register_file(to_integer(unsigned(RZ2)))) & unsigned(register_file(to_integer(unsigned(RA2))))) +
					(unsigned(register_file(to_integer(unsigned(RB2)))) & unsigned(register_file(to_integer(unsigned(RC2))))))(31 downto 0);
			elsif (Opcode2 = T2) then
				register_file(to_integer(unsigned(RX2))) <= std_logic_vector((unsigned(std_logic_vector(rotate_right(unsigned(register_file(to_integer(unsigned(RX2)))) & unsigned(register_file(to_integer(unsigned(RY2)))),28)) xor
						std_logic_vector(rotate_right(unsigned(register_file(to_integer(unsigned(RX2)))) & unsigned(register_file(to_integer(unsigned(RY2)))),34)) xor
						std_logic_vector(rotate_right(unsigned(register_file(to_integer(unsigned(RX2)))) & unsigned(register_file(to_integer(unsigned(RY2)))),39))) + 
							unsigned(((register_file(to_integer(unsigned(RX2))) & register_file(to_integer(unsigned(RY2)))) and (register_file(to_integer(unsigned(RZ2))) & register_file(to_integer(unsigned(RA2)))))xor
							((register_file(to_integer(unsigned(RX2))) & register_file(to_integer(unsigned(RY2)))) and (register_file(to_integer(unsigned(RB2))) & register_file(to_integer(unsigned(RC2)))))xor
							((register_file(to_integer(unsigned(RZ2))) & register_file(to_integer(unsigned(RA2)))) and (register_file(to_integer(unsigned(RB2))) & register_file(to_integer(unsigned(RC2))))))))(63 downto 32);
				register_file(to_integer(unsigned(RY2))) <= std_logic_vector((unsigned(std_logic_vector(rotate_right(unsigned(register_file(to_integer(unsigned(RX2)))) & unsigned(register_file(to_integer(unsigned(RY2)))),28)) xor
						std_logic_vector(rotate_right(unsigned(register_file(to_integer(unsigned(RX2)))) & unsigned(register_file(to_integer(unsigned(RY2)))),34)) xor
						std_logic_vector(rotate_right(unsigned(register_file(to_integer(unsigned(RX2)))) & unsigned(register_file(to_integer(unsigned(RY2)))),39))) + 
							unsigned(((register_file(to_integer(unsigned(RX2))) & register_file(to_integer(unsigned(RY2)))) and (register_file(to_integer(unsigned(RZ2))) & register_file(to_integer(unsigned(RA2)))))xor
							((register_file(to_integer(unsigned(RX2))) & register_file(to_integer(unsigned(RY2)))) and (register_file(to_integer(unsigned(RB2))) & register_file(to_integer(unsigned(RC2)))))xor
							((register_file(to_integer(unsigned(RZ2))) & register_file(to_integer(unsigned(RA2)))) and (register_file(to_integer(unsigned(RB2))) & register_file(to_integer(unsigned(RC2))))))))(31 downto 0);
--			
--			elsif (Opcode2= MLOAD0) then
--				message0 <= std_logic_vector(register_file(to_integer(unsigned(RX2))) & register_file(to_integer(unsigned(RY2))));
--				message1 <= std_logic_vector(register_file(to_integer(unsigned(RZ2))) & register_file(to_integer(unsigned(RA2))));
--				message2 <= std_logic_vector(register_file(to_integer(unsigned(RB2))) & register_file(to_integer(unsigned(RC2))));
--				message3 <= std_logic_vector(register_file(to_integer(unsigned(RD2))) & register_file(to_integer(unsigned(RE2))));
--			elsif (Opcode2= MLOAD1) then
--				message4 <= std_logic_vector(register_file(to_integer(unsigned(RX2))) & register_file(to_integer(unsigned(RY2))));
--				message5 <= std_logic_vector(register_file(to_integer(unsigned(RZ2))) & register_file(to_integer(unsigned(RA2))));
--				message6 <= std_logic_vector(register_file(to_integer(unsigned(RB2))) & register_file(to_integer(unsigned(RC2))));
--				message7 <= std_logic_vector(register_file(to_integer(unsigned(RD2))) & register_file(to_integer(unsigned(RE2))));	
			end if;			
			stage2 <= S2;
		when S2 =>
			if (Opcode2=ADD or Opcode2=SUB or Opcode2=IROR or Opcode2=IAND or
					 Opcode2=MUL or Opcode2=IOR or Opcode2=IXOR or Opcode2=ADI) then
				register_file(to_integer(unsigned(RX2))) <= ALU_out;
				Z <= zero;	S <= ALU_out(31);	V <= overflow;	C <= carry; --update CC
			elsif (Opcode2=CMP or Opcode2=CMPI) then
				Z <= zero;	S <= ALU_out(31);	V <= overflow;	C <= carry; --update CC only
			elsif (Opcode2=LDM or Opcode2=LDR or Opcode2=LDIX) then
				MDR_in <= MEM_in;
			elsif (Opcode2=STM or Opcode2=STR or Opcode2=STIX) then
				null;
			elsif (Opcode2=CALL or Opcode2=SYS) then
				MAR <= std_logic_vector(SP);	
				MDR_out <= std_logic_vector(PC);	
			elsif (Opcode2=RET or Opcode2=RETI or Opcode2=POP) then
				MDR_in <= MEM_IN;	SP <= SP - 1;
			elsif (Opcode2=PUSH) then
				MAR <= std_logic_vector(SP);
				MDR_out <= register_file(to_integer(unsigned(RX2)));
			elsif (Opcode2 = T11) then
				register_file(to_integer(unsigned(RD2))) <= std_logic_vector(tmpx);
				register_file(to_integer(unsigned(RE2))) <= std_logic_vector(tmpy);
			elsif (Opcode2=STIX64) then
					MAR <= std_logic_vector(unsigned(
						register_file(to_integer(unsigned(RX2))))
						+ (unsigned(M2)+1)); MDR_out <= register_file(to_integer(unsigned(RZ2)));		
			end if;
			stage2 <= S1;
		when others =>
			null;
	end case;
	
	case stage3 is
		when S1 =>
			if (Opcode3=LDM or Opcode3=LDR or Opcode3=LDIX) then
				register_file(to_integer(unsigned(RX3))) <= MDR_in;
			elsif (Opcode3=STM or Opcode3=STR or Opcode3=STIX or Opcode3=STIX64) then
				null;
			elsif (Opcode3=CALL) then
				PC <= x"000" & unsigned(M3);
			elsif (Opcode3=POP) then
				register_file(to_integer(unsigned(RX3))) <= MDR_in;
			elsif (Opcode3=RET) then
				PC <= unsigned(MDR_in);
			elsif (Opcode3=RETI) then
				PSW <= MDR_in;	MAR <= std_logic_vector(SP);	
			elsif (Opcode3=PUSH) then
				null;	
			elsif (Opcode3=SYS) then
				SP <= SP + 1;
			end if;
			stage3 <= S2;
		when S2 =>
			if (Opcode3=RETI) then
				MDR_in <= MEM_IN;	sp <= sp - 1;
			elsif (Opcode3=SYS) then
				MAR <= std_logic_vector(SP);
				MDR_out <= PSW;
			end if;
			stage3 <= S1;
		when others =>
			null;
	end case;
	
	case stage4 is
		when S1 =>
			if (Opcode4=RETI) then
				PC <= unsigned(MDR_in);
			elsif (Opcode4=SYS) then
				PC <= X"000FFC0"&unsigned(IR4(3 downto 0));
			else	stage4 <= S2;
			end if;
			stage4 <= S2;
		when S2 =>
			stage4 <= S1;
		when others =>
			null;
	end case;
end if;
end process;
--------------------ALU----------------------------			
Rhody_ALU: entity work.alu port map(
		alu_op => IR2(28 downto 26), 
		operand0 => operand0, 
		operand1 => operand1,
		n => IR2(4 downto 0), 
		alu_out => ALU_out,
		carry => carry,
		overflow => overflow);	
zero <= '1' when alu_out = X"00000000" else '0';	
operand0 <= register_file(to_integer(unsigned(RX2)));
-----------------------------------------------------
end Structural;

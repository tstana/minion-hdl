library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use ieee.numeric_std.all;

entity testbench_two_or_more_bits_high is
end testbench_two_or_more_bits_high;

architecture arch of testbench_two_or_more_bits_high is

  constant c_clk_per : time := 20ns;

  function f_two_or_more_bits_set(v : std_logic_vector(5 downto 0))
        return std_logic is
    variable ret : std_logic;
  begin

    ret := v(1) and v(0);

    for i in 2 to 5 loop
      for j in 0 to i-1 loop
        ret := ret or (v(i) and v(j));
      end loop;
    end loop;

    return ret;

  end function;

  signal clk  : std_logic             := '0';
  signal stim : unsigned(5 downto 0)  := (others => '0');
  signal outp : std_logic;

  signal alarm : std_logic            := '0';

begin

  p_clk : process
  begin
    clk <= '0';
    wait for c_clk_per / 2;
    clk <= '1';
    wait for c_clk_per / 2;
  end process p_clk;

  p_stim : process (clk)
  begin
    if rising_edge(clk) then
      stim <= stim + 1;
    end if;
  end process;

  outp <= f_two_or_more_bits_set(std_logic_vector(stim));

  alarm <= '1' when ((stim = 32) or (stim = 16) or (stim = 8) or (stim = 4) or (stim = 2)
                      or (stim = 1))
                    and (outp = '1') else
           '0';

end arch;


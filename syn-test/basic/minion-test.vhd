library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity minion_test is
  port
  (
    clk_i : in  std_logic;
    div_o : out std_logic;
    dc_o  : out std_logic
  );
end entity minion_test;

architecture arch of minion_test is

  signal count : unsigned(4 downto 0);
  signal div   : std_logic;

begin

  process (clk_i) is
  begin
    if rising_edge(clk_i) then
      count <= count + 1;
      if (count = 24) then
        div   <= not div;
        count <= (others => '0');
      end if;
    end if;
  end process;

  dc_o <= '1';
  div_o <= div;

end architecture;

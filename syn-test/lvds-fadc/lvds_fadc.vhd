library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity lvds_fadc is
  port
  (
    clk_i       : in  std_logic;
    dip_n_i     : in  std_logic_vector(7 downto 0);
    led_in_n_o  : out std_logic_vector(7 downto 0);
    led_out_n_o : out std_logic_vector(7 downto 0);
    lvds_i      : in  std_logic_vector(7 downto 0);
    lvds_o      : out std_logic_vector(7 downto 0)
  );
end lvds_fadc;

architecture arch of lvds_fadc is

  signal blink : std_logic;
  signal count : unsigned(24 downto 0);

begin

  p_blinky : process (clk_i)
  begin
    if rising_edge(clk_i) then
      count <= count + 1;
      if (count = 24999999) then
        count <= (others => '0');
        blink <= not blink;
      end if;
    end if;
  end process p_blinky;

gen_outps: for i in 0 to 7 generate
  led_out_n_o(i) <= not blink when dip_n_i(i) = '0' else '1';
  lvds_o(i)      <= blink when dip_n_i(i) = '0' else '0';
end generate gen_outps;

  led_in_n_o  <= not lvds_i;

end arch;


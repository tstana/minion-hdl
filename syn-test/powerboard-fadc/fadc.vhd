library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity fadc is
  port
  (
    dip_n_i     : in  std_logic_vector(7 downto 0);
    led_in_n_o  : out std_logic_vector(7 downto 0);
    led_out_n_o : out std_logic_vector(7 downto 0);
    lvds_i      : in  std_logic_vector(7 downto 0);
    lvds_o      : out std_logic_vector(7 downto 0)
  );
end fadc;

architecture arch of fadc is

begin

gen_outps: for i in 0 to 7 generate
  led_out_n_o(i) <= '0' when dip_n_i(i) = '0' else '1';
  lvds_o(i)      <= '1' when dip_n_i(i) = '0' else '0';
end generate gen_outps;

  led_in_n_o  <= not lvds_i;

end arch;

